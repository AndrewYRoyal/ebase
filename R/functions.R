#' Data Format
#' @import data.table
#' @export
ebDataFormat <- function(useDT, meterDT, base.length, date.format, padding,
                         base.min = NULL, perf.min = NULL, interval){
  useDT <- copy(useDT)
  meterDT <- copy(meterDT)

  if(is.character(useDT[, date])) useDT[, date:= as.POSIXct(date, format = date.format, tz = 'UTC')]
  if(is.character(meterDT[, inDate])) meterDT[, inDate:= as.POSIXct(inDate, format = date.format, tz = 'UTC')]
  for(meter in unique(meterDT[, meterID])){
    meterDT[meterID == meter,
            inStart:= seq(from = inDate,
                          length = 2,
                          by = paste0("-", padding, " days"))[2]]
    meterDT[meterID == meter,
            inEnd:= seq(from = inDate,
                        length = 2,
                        by = paste0("+", padding, " days"))[2]]
    meterDT[meterID == meter,
            blStart:= seq(from = inStart,
                          length = 2,
                          by = paste0("-", base.length, " months"))[2]]
    meterDT[meterID == meter,
            pEnd:= seq(inEnd,
                       length = 2,
                       by = paste0("+", 12, " months"))[2]]
  }
  datVarV <- intersect(
    names(meterDT),
    c('inStart', 'inEnd', 'blStart', 'pEnd'))
  useDT <- merge(
    useDT,
    meterDT[, c('meterID', datVarV), with = FALSE],
    by = 'meterID')
  useDT <- useDT[date >= blStart & date <= pEnd, ]
  useDT[, period:=
          as.numeric(date >= blStart) +
          as.numeric(date >= inStart) +
          as.numeric(date >= inEnd)]
  useDT[, c('period', 'mm'):= list(
    c('baseline', 'install', 'performance')[period],
    month(date))]

  baseDropV <- NULL
  perfDropV <- NULL
  getDays <- function(x) uniqueN(as.POSIXct(round(x, 'days')))
  if(!is.null(base.min)){
    baseDropV <- useDT[period == 'baseline', getDays(date) > base.min, by = .(meterID)][V1 == FALSE, meterID]
    baseDropV <- c(baseDropV, setdiff(unique(useDT[, meterID]), unique(useDT[period == 'baseline', meterID])))
  }
  if(!is.null(perf.min)){
    perfDropV <- useDT[period == 'performance', getDays(date) > perf.min, by = .(meterID)][V1 == FALSE, meterID]
    perfDropV <- c(perfDropV, setdiff(unique(useDT[, meterID]), unique(useDT[period == 'performance', meterID])))
  }
  useDT <- useDT[!(meterID %in% c(baseDropV, perfDropV)), ]
  useDT[, tow:= .GRP, by = .(wday(date), hour(date))]
  useDT[, (datVarV):= NULL]
  meterV <- unique(useDT[, meterID]); names(meterV) <- meterV

  classV <- c('hourly' = 'ebHourly')
  out <- structure(list(dat = useDT,
                        meterV = meterV,
                        baseDropV = baseDropV,
                        perfDropV = perfDropV),
                   class = classV[interval])

  cat(length(meterV), 'total meters \n',
      length(baseDropV),'insufficient baseline \n',
      length(perfDropV),'insufficient performance \n')
  return(out)
}

#' Model
#' @export
ebModel <- function(ebDat, type, option) UseMethod('ebModel')

#' Hourly Model
#' @import data.table
#' @import mlr
#' @export
ebModel.ebHourly <- function(ebDat, type = 'reg', option = NULL){
  if(!(type %in% c('reg', 'GBoost'))) stop('Improper model type selected.')
  dat <- copy(ebDat$dat)
  if(type == 'reg'){
    varV <- c('elct', paste0('tbin', 1:11), paste0('mm', 1:12))
    dat <- ebRegFormat(dat)
    modelList <- lapply(ebDat$meterV,
                        function(meter) regModel(meter = meter, dat = dat, varV = varV))
    structure(list(modelList = modelList, varV = varV), class = 'reg')
  } else if(type == 'GBoost'){
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

    parallelMap::parallelStart(mode = 'socket', cpus = cpus, level = 'mlr.tuneParams')
    suppressWarnings({
      suppressMessages(
        modelList <- lapply(ebDat$meterV, function(meter) gboost(meter, dat = dat, pSet = pSet))
      )
    })
    parallelMap::parallelStop()
    structure(list(modelList = modelList), class = 'GBoost')
  }
}

#' GBoost Model Helper
#' @import data.table
#' @import mlr
#' @export

gboost <- function(meter, dat, pSet, option){
  x <- dat[meterID == meter, ]
  t <- length(x[period == 'baseline', date])
  blockFactor <- factor(sort(rep(1:pSet$blocks, t)[1:t]))
  regTask <- makeRegrTask(id = 'reg',
                          data = as.data.frame(x[period == 'baseline',
                                                 -c('meterID', 'date', 'period')]),
                          target = 'elct',
                          blocking = blockFactor)
  paramSpace <- makeParamSet(
    makeDiscreteParam('max_depth', values = pSet$max_depth),
    makeDiscreteParam('nrounds', values = pSet$nrounds),
    makeDiscreteParam('early_stopping_rounds', values = pSet$early_stopping_rounds),
    makeDiscreteParam('eta', values = pSet$eta))
  ctrl <- makeTuneControlGrid()
  rSampleDesc <- makeResampleDesc('CV', iter = 2)
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
  xgbModel
}




#' Predictions
#' @export
ebPredict <- function(x, ebDT) UseMethod('ebPredict')

#' Reg Prediction
#' @import data.table
#' @export
ebPredict.reg <- function(mList, ebDat){
  dat <- copy(ebDat$dat)
  dat <- ebRegFormat(dat)
  predList <- lapply(ebDat$meterV,
                     function(meter){
                       ebRPred(meter = meter,
                               dat = dat,
                               varV = mList$varV,
                               mList = mList)
                     })
  rbindlist(predList)
}

#' Reg Format Helper
#' @import data.table
#' @export
ebRegFormat <- function(dat){
  tBreaksV <- c(-Inf, 45 + 0:9*5, Inf)
  dat[, tbin:= cut(temp, breaks = tBreaksV, labels = 1:11)]
  dat[, paste0('tbin', 1:11):= lapply(1:11, function(t) as.numeric(tbin == t))]
  dat[, paste0('mm', 1:12):= lapply(1:12, function(m) as.numeric(mm == m))]
  dat
}

#' Reg Model Helper
#' @import data.table
#' @export
regModel <- function(meter, dat, varV){
  regDT <- dat[meterID == meter & period == 'baseline', ]
  mod <- lm(elct ~ . - tow,
            data = regDT[, lapply(.SD, function(x) x - mean(x)),
                         .SDcols = varV,
                         by = .(tow)])
  regDT <- regDT[, lapply(.SD, mean),
                 .SDcols = varV,
                 by = .(tow)]
  return(list(mod = mod, meanDT = regDT, varV = varV))
}

#' GBoost Predict
#' @import data.table
#' @import mlr
#' @export
ebPredict.GBoost <- function(mList, ebDat){
  dat <- copy(ebDat$dat)
  predList <- lapply(
    ebDat$meterV,
    function(meter){
      x <- dat[meterID == meter, ]
      x[meterID == meter,
        pElct:= predict(mList$modelList[[meter]],
                        newdata = as.data.frame(x[, -c('meterID', 'elct', 'date', 'period')]))$data$response]
      x
    }
  )
  rbindlist(predList)[, .(meterID, date, period, elct, pElct)]
}

#' Regression Predict Helper
#' @import data.table
#' @export
ebRPred <- function(meter, dat, varV, mList){
  predictDT <- merge(
    dat[meterID == meter, ],
    mList$modelList[[meter]][['meanDT']],
    by = 'tow',
    suffixes = c('', '.mean'))
  predictDT[, (varV):= lapply(varV, function(v) get(v) - get(paste0(v, '.mean')))]

  predictDT[, pElct:= elct.mean + predict(mList$modelList[[meter]][['mod']], predictDT)]
  merge(dat[meterID == meter, .(meterID, date, period, elct)],
        predictDT[, .(date, pElct)],
        by = 'date')[, .(meterID, date, period, elct, pElct)]
}

#' Prediction and Savings Summary
#' @import data.table
#' @export
ebSummary <- function(predDT, meterDT){
  predDT <- copy(predDT)
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
    actual <- na.omit(actual); predicted <- na.omit(predicted)
    sum((actual - mean(actual))^2)/adjustedN(actual, length(actual)) +
      sum((predicted - mean(predicted))^2)/adjustedN(predicted, length(predicted))
  }
  adjustedN <- function(x, n){
    corr <- acf(x, lag.max = 1, plot = FALSE)$acf[2]
    (1 - corr)/(1 + corr)
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


#' Data Print Method
#' @import data.table
#' @export
print.ebHourly <- function(x){
  hpm <- x$dat[, .N, by = .(meterID)]$N
  cat(length(x$meterV),'meters \n',
      'Hours per meter: \n',
      'Mean ', mean(hpm), 'Min', min(hpm), 'Max ', max(hpm))
}




