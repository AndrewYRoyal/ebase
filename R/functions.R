#' Data Format
#' @import data.table
#' @export
ebDataFormat <- function(useDT,
                         meterDT,
                         base.length,
                         date.format,
                         interval = c('daily', 'hourly'),
                         padding = 0,
                         base.min = 0,
                         perf.min = 0){
  if(is.character(useDT[, date])) useDT[, date:= as.POSIXct(date, format = date.format, tz = 'UTC')]
  if(is.character(meterDT[, inDate])) meterDT[, inDate:= as.POSIXct(inDate, format = date.format, tz = 'UTC')]
  interval <- match.arg(interval)
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
ebMeterFormat <- function(meter, useDT, meterDT, base.length, date.format, padding, base.min,
                          perf.min, interval){
  dat <- useDT[meterID == meter, ]
  inDate <- meterDT[meterID == meter, inDate]
  dat[, period:=
        (date >= inDate - as.difftime(padding + base.length, units = 'days')) +
        (date >= inDate - as.difftime(padding, units = 'days')) +
        (date >= inDate + as.difftime(padding, units = 'days')) +
        (date >= inDate + as.difftime(padding + 365, units = 'days'))]
  dat <- dat[period > 0 & period < 4, ]
  pNames <- c('baseline', 'install', 'performance')
  dat[, period:= as.factor(pNames[period])]
  dat[, tow:= .GRP, by = .(wday(date), hour(date))]
  dat[, mm:= month(date)]

  getDays <- function(x) uniqueN(as.POSIXct(round(x, 'days')))
  out <- list(dat = list(baseline = dat[period == 'baseline', ],
                         install = dat[period == 'install', ],
                         performance = dat[period == 'performance', ]),
              property = unique(meterDT[meterID == meter, propertyName]),
              model = getDays(dat[period == 'baseline', date]) >= base.min &
                getDays(dat[period == 'performance', date]) >= perf.min)

  classV <- list('hourly' = 'hourly', 'daily' = c('daily', 'hourly'))
  structure(out, class = classV[[interval]])
}

#' Baseline Model
#' @import data.table
#' @import mlr
#' @export
ebModel <- function(dataList, type = 'regress', option = NULL){
  if(!(type %in% c('regress', 'gboost'))) stop('Error: Type not recognized.')
  if(type == 'regress'){
    modelList <- lapply(dataList, function(x){
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
        modelList <- lapply(dataList, function(x){
          if(!x$model) return(NA)
          gboost(x, pSet = pSet)
        })
      )
    })
    parallelMap::parallelStop()
  }
  modelList
}

#' Predictions
#' @import data.table
#' @import mlr
#' @export
ebPredict <- function(modelList, dataList, period = c('performance', 'baseline')){
  meterDict <- names(modelList); names(meterDict) <- meterDict
  propertyDict <- setNames(sapply(dataList, function(x) x$property),
                           names(dataList))
  period <- match.arg(period)

  dat <- lapply(meterDict,
                function(x){
                  tryCatch(predict(modelList[[x]], dataList[[x]]))#,
                           #error = function(e) data.table(meterID = x))
                  })
  return(list(dat = dat,
              propertyDict = propertyDict))
}

#' Regression Predict
#' @import data.table
#' @export
predict.regress <- function(x, dat){
  if(!dat$model) return(NA)
  varV <- x$varV
  dat <- regFormat(rbindlist(dat$dat))
  predictDT <- merge(
    dat,
    x$meanDT,
    by = 'tow',
    suffixes = c('', '.mean'))
  predictDT[, (varV):= lapply(varV, function(v) get(v) - get(paste0(v, '.mean')))]
  predictDT[, pUse:= use.mean + predict(x$mod, predictDT)]
  merge(
    dat[, .(meterID, date, period, use)],
    predictDT[, .(date, pUse)],
    by = 'date')[, .(meterID, date, period, use, pUse)]
}

#' GBoost Predict
#' @import data.table
#' @import mlr
#' @export
predict.gboost <- function(x, dat){
  if(!dat$model) return(NA)
  dat <- copy(rbindlist(dat$dat))
  dat[, pUse:= predict(x$mod, newdata = as.data.frame(
    dat[, intersect(names(dat), c('use', 'temp', 'tow', 'mm')), with = FALSE]))$data$response]
  dat[, .(meterID, date, period, use, pUse)]
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

#' Baseline Summary Helper
#' @import data.table
#' @export
ebBaselineSummary <- function(dat){
  dat[period == 'baseline', list(
    meterID = unique(meterID),
    R2 = R2(use, pUse),
    CVRMSE = CVRMSE(use, pUse),
    NMBE = NMBE(use, pUse),
    baselineDays = uniqueN(as.POSIXct(round(date, 'days'))),
    Annual = sum(use))]
}
#' Saving Summary Helper
#' @import data.table
#' @export
ebSavingSummary <- function(dat){
  dat[period == 'performance', list(
    meterID = unique(meterID),
    Savings = sum(pUse - use),
    varSavings = varCalc(use, pUse),
    performanceDays = uniqueN(as.POSIXct(round(date, 'days'))))]
}

#' Prediction and Savings Summary
#' @import data.table
#' @export
ebSummary <- function(predictions){
  metricsList <- lapply(predictions$dat, function(x){
    ebBaselineSummary(x)
  })
  savingList <- lapply(predictions$dat, function(x){
    ebSavingSummary(x)
  })

  propDT <- rbindlist(
    lapply(predictions$dat, function(dat){
      dat[, propertyName:= predictions$propertyDict[meterID]]
      dat
    })
  )
  propMetrics <- rbindlist(
    lapply(unique(predictions$propertyDict), function(prop){
      dat <- ebBaselineSummary(propDT[propertyName == prop, ])
      dat[, meterID:= NULL]
      dat[, propertyName:= prop]
      unique(dat)
    })
  )
  propSavings <- rbindlist(
    lapply(unique(predictions$propertyDict), function(prop){
      dat <- ebSavingSummary(propDT[propertyName == prop, ])
      dat[, meterID:= NULL]
      dat[, propertyName:= prop]
      unique(dat)
    })
  )
  return(list(Meters = list(metrics = rbindlist(metricsList),
                            savings = rbindlist(savingList)),
              Properties = list(metrics = propMetrics,
                                savings = propSavings)))
}
#' Plot Meter Data
#' @import data.table
#' @import dygraphs
#' @export
ebPlot <- function(x, compress = TRUE){
  dat <- copy(x)
  setnames(dat, c('use', 'pUse'), c('Actual', 'Predicted'))
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

#' R2 calculation
#' @export
R2 <- function(actual, predicted){
  actual <- na.omit(actual); predicted <- na.omit(predicted)
  round(1 - sum((actual - predicted)^2)/sum((actual - mean(actual))^2), 2)
}
#' CVRMSE calculation
#' @export
CVRMSE <- function(actual, predicted){
  actual <- na.omit(actual); predicted <- na.omit(predicted)
  round(100*sqrt(mean((actual - predicted)^2))/mean(actual), 0)
}
#' NMBE calculation
#' @export
NMBE <- function(actual, predicted){
  actual <- na.omit(actual); predicted <- na.omit(predicted)
  format(100*sum(predicted - actual)/sum(actual), digits = 1)
}
#' Variance calculation
#' @export
varCalc <- function(actual, predicted){
  adjustedN(actual, length(actual))
  actual <- na.omit(actual); predicted <- na.omit(predicted)
  sum((actual - mean(actual))^2)/adjustedN(actual, length(actual)) +
    sum((predicted - mean(predicted))^2)/adjustedN(predicted, length(predicted))
}
#' Serial-Corr. Correction
#' @export
adjustedN <- function(x, n){
  corr <- acf(x, lag.max = 1, plot = FALSE)$acf[2]
  n*(1 - corr)/(1 + corr)
}
