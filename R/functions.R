#' Data Format
#' @import data.table
#' @export
ebDataFormat <- function(useDT, meterDT, base.length, date.format, padding,
                         base.min = NULL, perf.min = NULL, interval){
  if(is.character(useDT[, date])) useDT[, date:= as.POSIXct(date, format = date.format, tz = 'UTC')]
  if(is.character(meterDT[, inDate])) meterDT[, inDate:= as.POSIXct(inDate, format = date.format, tz = 'UTC')]
  if(is.null(base.min)) base.min <- 0
  if(is.null(perf.min)) perf.min <- 0
  meterV <- unique(meterDT$meterID); names(meterV) <- meterV

  ebData <- lapply(
    meterV,
    function(m) {
      ebMeterFormat(meter = m,
                    meterDT = meterDT,
                    useDT = useDT,
                    base.length = base.length,
                    padding = padding,
                    base.min = base.min,
                    perf.min = perf.min,
                    interval = interval)
    })

  cat(length(meterV), 'total meters \n',
      sum(sapply(ebData, function(x) x[['model']])), 'with sufficient data \n')
  ebData
}

#' Meter Format
#' @import data.table
#' @export
ebMeterFormat <- function(meter, useDT, meterDT, base.length, date.format, padding, base.min = NULL,
                          perf.min = NULL, interval){
  dat <- useDT[meterID == meter, ]
  inDate <- meterDT[meterID == meter, inDate]
  dat[, period:=
        (date >= inDate - as.difftime(padding, units = 'days') - as.difftime(30*base.length, units = 'days')) +
        (date >= inDate - as.difftime(padding, units = 'days')) +
        (date >= inDate + as.difftime(padding, units = 'days'))]
  dat <- dat[period > 0, ]
  pNames <- c('baseline', 'install', 'performance')
  dat[, period:= as.factor(pNames[period])]
  dat[, tow:= .GRP, by = .(wday(date), hour(date))]
  dat[, mm:= month(date)]

  getDays <- function(x) uniqueN(as.POSIXct(round(x, 'days')))
  out <- list(dat = dat,
              model = getDays(dat[period == 'baseline', date]) >= base.min &
                getDays(dat[period == 'performance', date]) >= perf.min)

  classV <- c('hourly' = 'hourly')
  structure(out, class = classV[interval])
}

#' Baseline Model
#' @import data.table
#' @import mlr
#' @export
ebModel <- function(ebData, type = 'regress', option = NULL){
  if(!(type %in% c('regress', 'gboost'))) stop('Error: Type not recognized.')
  if(type == 'regress'){
    modelList <- lapply(ebData, function(x){
      if(!x$model) return(NA)
      regress(x)
    })
  }
  if(type == 'gboost'){
    pSet <- list(max_depth = 3,
                 nrounds = seq(200, 1400, 200),
                 early_stopping_rounds = 5,
                 eta = 0.1,
                 blocks = 2,
                 cpus = 4)
    if(!is.null(option$max_depth)) pSet$max_depth <- option$max_depth
    if(!is.null(option$nrounds)) pSet$nrounds <- option$nrounds
    if(!is.null(option$early_stopping_rounds)) pSet$early_stopping_rounds <- option$early_stopping_rounds
    if(!is.null(option$eta)) pSet$eta <- option$eta
    if(!is.null(option$blocks)) pSet$blocks <- option$blocks
    if(!is.null(option$cpus)) pSet$cpus <- option$cpus

    parallelMap::parallelStart(mode = 'socket', cpus = pSet$cpus, level = 'mlr.tuneParams')
    suppressWarnings({
      suppressMessages(
        modelList <- lapply(ebData, function(x){
          if(!x$model) return(NA)
          gboost(x, pSet = pSet)
        })
      )
    })
    parallelMap::parallelStop()
  }
  modelList
}

#' Regression Method
#' @import data.table
#' @export
regress <- function(x) UseMethod('regress')

#' Hourly Regression
#' @import data.table
#' @export
regress.hourly <- function(x){
  varV <- c('elct', paste0('tbin', 1:11), paste0('mm', 1:12))
  dat <- x$dat[period == 'baseline', ]
  dat <- regFormat(dat)
  mod <- lm(elct ~ . - tow,
            data = dat[, lapply(.SD,
                                function(x) x - mean(x)),
                       .SDcols = varV,
                       by = .(tow)])
  dat <- dat[, lapply(.SD, mean),
             .SDcols = varV,
             by = .(tow)]
  out <- list(mod = mod, meanDT = dat, varV = varV)
  structure(out, class = 'regress')
}

#' Gradient Boost
#' @import data.table
#' @import mlr
#' @export
gboost <- function(x, pSet) UseMethod('gboost')


#' Hourly Gradient Boost
#' @import data.table
#' @import mlr
#' @export
gboost.hourly <- function(x, pSet){
  dat <- copy(x$dat)
  t <- length(dat[period == 'baseline', date])
  blockFactor <- factor(sort(rep(1:pSet$blocks, t)[1:t]))
  regTask <- makeRegrTask(id = 'reg',
                          data = as.data.frame(
                            dat[period == 'baseline',
                              -c('meterID', 'date', 'period')]),
                          target = 'elct',
                          blocking = blockFactor)
  paramSpace <- makeParamSet(
    makeDiscreteParam('max_depth', values = pSet$max_depth),
    makeDiscreteParam('nrounds', values = pSet$nrounds),
    makeDiscreteParam('early_stopping_rounds', values = pSet$early_stopping_rounds),
    makeDiscreteParam('eta', values = pSet$eta))
  ctrl <- makeTuneControlGrid()
  rSampleDesc <- makeResampleDesc('CV', iter = pSet$blocks)
  tuner <- tuneParams(
    learner = 'regr.xgboost',
    task = regTask,
    resampling = rSampleDesc,
    par.set = paramSpace,
    control = ctrl,
    show.info = FALSE)
  xgbLearn <- setHyperPars(
    makeLearner('regr.xgboost', verbose = 0, nthread = 8),
    par.vals = tuner$x)
  xgbModel <- train(learner = xgbLearn, task = regTask)
  structure(list(mod = xgbModel), class = 'gboost')
}

#' Predictions
#' @import data.table
#' @import mlr
#' @export
ebPredict <- function(ebModels, ebData){
  meterV <- names(ebModels); names(meterV) <- meterV
  lapply(meterV,
         function(x){
           tryCatch(predict(ebModels[[x]], ebData[[x]]), error = function(e) data.table(meterID = x))
         })
}

#' Regression Predict
#' @import data.table
#' @export
predict.regress <- function(x, dat){
  if(!dat$model) return(NA)
  varV <- x$varV
  dat <- regFormat(dat$dat)
  predictDT <- merge(
    dat,
    x$meanDT,
    by = 'tow',
    suffixes = c('', '.mean'))
  predictDT[, (varV):= lapply(varV, function(v) get(v) - get(paste0(v, '.mean')))]
  predictDT[, pElct:= elct.mean + predict(x$mod, predictDT)]
  merge(
    dat[, .(meterID, date, period, elct)],
    predictDT[, .(date, pElct)],
    by = 'date')[, .(meterID, date, period, elct, pElct)]
}

#' GBoost Predict
#' @import data.table
#' @import mlr
#' @export
predict.gboost <- function(x, dat){
  if(!dat$model) return(NA)
  dat <- copy(dat$dat)
  dat[, pElct:= predict(x$mod, newdata = as.data.frame(
    dat[, -c('meterID', 'elct', 'date', 'period')]))$data$response]
  dat[, .(meterID, date, period, elct, pElct)]
}

#' Reg Format Helper
#' @import data.table
#' @export
regFormat <- function(dat){
  tBreaksV <- c(-Inf, 45 + 0:9*5, Inf)
  dat[, tbin:= cut(temp, breaks = tBreaksV, labels = 1:11)]
  dat[, paste0('tbin', 1:11):= lapply(1:11, function(t) as.numeric(tbin == t))]
  dat[, paste0('mm', 1:12):= lapply(1:12, function(m) as.numeric(mm == m))]
  dat
}


# TODO: refactor prediction summary
#' Prediction and Savings Summary
#' @import data.table
#' @export
ebSummary <- function(predictions, meterDT){
  predDT <- rbindlist(predictions, fill = TRUE)
  meterDT <- copy(meterDT)
  R2 <- function(actual, predicted){
    actual <- na.omit(actual); predicted <- na.omit(predicted)
    round(1 - sum((actual - predicted)^2)/sum((actual - mean(actual))^2), 2)
  }
  CVRMSE <- function(actual, predicted){
    actual <- na.omit(actual); predicted <- na.omit(predicted)
    round(100*sqrt(mean((actual - predicted)^2))/mean(actual), 0)
  }
  NMBE <- function(actual, predicted){
    actual <- na.omit(actual); predicted <- na.omit(predicted)
    format(100*sum(predicted - actual)/sum(actual), digits = 1)
  }
  varCalc <- function(actual, predicted){
    adjustedN(actual, length(actual))
    actual <- na.omit(actual); predicted <- na.omit(predicted)
    sum((actual - mean(actual))^2)/adjustedN(actual, length(actual)) +
      sum((predicted - mean(predicted))^2)/adjustedN(predicted, length(predicted))
  }
  adjustedN <- function(x, n){
    corr <- acf(x, lag.max = 1, plot = FALSE)$acf[2]
    n*(1 - corr)/(1 + corr)
  }

  sumMeterDT <- merge(
    meterDT[, .(meterID)],
    predDT[period == 'baseline',
           list(
             R2 = R2(elct, pElct),
             CVRMSE = CVRMSE(elct, pElct),
             NMBE = NMBE(elct, pElct),
             baselineDays = round(.N/24, 0),
             AnnualkWh = sum(elct)),
           by = .(meterID)],
    all.x = TRUE, by = 'meterID')

  sumMeterDT <- merge(
    sumMeterDT,
    predDT[period == 'performance',
           list(
             Savings = sum(pElct - elct),
             varSavings = varCalc(elct, pElct),
             performanceDays = round(.N/24, 0)),
           by = .(meterID)],
    all.x = TRUE, by = 'meterID')

  sumPropertyDT <- NA
  if('propertyName' %in% names(meterDT)){
    sumPropertyDT <- merge(
      meterDT[, .(meterID, propertyName)],
      predDT,
      all.x = TRUE, by = 'meterID')[period == 'baseline', lapply(.SD, sum),
                                    .SDcols = c('elct', 'pElct'),
                                    by = .(propertyName, date)]

    sumPropertyDT <- sumPropertyDT[, list(
      R2 = R2(elct, pElct),
      CVRMSE = CVRMSE(elct, pElct),
      NMBE = NMBE(elct, pElct),
      AnnualkWh = sum(elct)),
      by = .(propertyName)]

    sumPropertyDT <- merge(
      sumPropertyDT,
      merge(
        sumMeterDT[, .(meterID, Savings, varSavings)],
        meterDT[, .(propertyName, meterID)])[, lapply(.SD, sum, na.rm = TRUE),
                                             .SDcols = c('Savings', 'varSavings'),
                                             by = .(propertyName)],
      all.x = TRUE, all.y = TRUE, by = 'propertyName')
  }
  return(list(
    summaryMeter = sumMeterDT,
    summaryProperty = sumPropertyDT))
}

#' Plot Meter Data
#' @import data.table
#' @import dygraphs
#' @export
ebPlot <- function(x, compress = TRUE){
  dat <- copy(x)
  setnames(dat, c('elct', 'pElct'), c('Actual', 'Predicted'))
  datesV <- c(
    min(dat[period == 'baseline', date]),
    max(dat[period == 'baseline', date]),
    min(dat[period == 'performance', date]),
    max(dat[period == 'performance', date]))
  if(compress) dat <- dat[, lapply(.SD, sum),
                          .SDcols = c('Actual', 'Predicted'),
                          by = .(date = as.POSIXct(trunc.POSIXt(date, 'days', tz = 'UTC'), tz = 'UTC'))]
  dygraph(dat[, .(date, Actual, Predicted)], ylab = 'Daily kWh') %>%
    dySeries("Actual", stepPlot = TRUE, fillGraph = TRUE, color = '#4889ce') %>%
    dySeries("Predicted", strokeWidth = 1, stepPlot = TRUE, color = 'black') %>%
    dyShading(from = datesV[3], to = datesV[4], color = "#CCEBD6") %>%
    dyEvent(datesV[2]) %>%
    dyEvent(datesV[3], 'Install Period', labelLoc = "top") %>%
    dyLegend(width = 400)
}

#' Data Print Method
#' @import data.table
#' @export
print.hourly <- function(x){
  obs <- uniqueN(x$dat)
  cat(obs,
      'Observations \n',
      'Type: Hourly \n',
      'Modeled', x$model)
}

