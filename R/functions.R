#' Data Format
#' @import data.table
#' @export
ebDataFormat <- function(useDT,
                         meterDT,
                         base.length = 365,
                         perf.length = 365,
                         interval = c('daily', 'hourly'),
                         padding = 0,
                         base.min = 0,
                         perf.min = 0){
  interval <- match.arg(interval)
  meterDict <- setNames(unique(meterDT$meterID), unique(meterDT$meterID))
  cat(length(meterDict), 'total meters \n')

  dataList <- lapply(
    meterDict,
    function(m){
      ebMeterFormat(meter = m,
                    meterDT = meterDT,
                    useDT = useDT,
                    base.length = base.length,
                    perf.length = perf.length,
                    padding = padding,
                    base.min = base.min,
                    perf.min = perf.min,
                    interval = interval)
  })

  meterDict <- meterDict[sapply(meterDict, function(meter) dataList[[meter]][['model']])]
  cat(length(meterDict), 'with sufficient data \n')

  out <- list(
    baseline = lapply(meterDict, function(meter) dataList[[meter]][['baseline']]),
    install = lapply(meterDict, function(meter) dataList[[meter]][['install']]),
    performance = lapply(meterDict, function(meter) dataList[[meter]][['performance']]),
    meterDict = meterDict,
    propertyDict = sapply(meterDict, function(meter) dataList[[meter]][['property']]),
    ivars = lapply(meterDict, function(meter) dataList[[meter]][['ivars']]),
    tcuts = lapply(meterDict, function(meter) dataList[[meter]][['tcuts']]))
  out
}

#' Meter Format
#' @import data.table
#' @export
ebMeterFormat <- function(meter, useDT, meterDT, base.length, date.format, padding, base.min,
                          perf.min, perf.length, interval){
  dat <- useDT[meterID == meter, ]
  inDate <- meterDT[meterID == meter, inDate]
  dat[, period:=
        (date >= inDate - as.difftime(padding + base.length, units = 'days')) +
        (date >= inDate - as.difftime(padding, units = 'days')) +
        (date >= inDate + as.difftime(padding, units = 'days')) +
        (date >= inDate + as.difftime(padding + perf.length, units = 'days'))]
  dat <- dat[period > 0 & period < 4, ]
  pNames <- c('baseline', 'install', 'performance')
  dat[, period:= as.factor(pNames[period])]
  dat[, tow:= .GRP, by = .(wday(date), hour(date))]
  dat[, month:= month(date)]
  dat[, mm:= as.factor(month)]
  tcuts <- c(-Inf, quantile(dat$temp, 1:10/10))
  dat[, tbin:= as.factor(cut(temp, tcuts))]

  getDays <- function(x) uniqueN(as.POSIXct(round(x, 'days')))
  classV <- list('hourly' = c('hourly', 'data.table'), 'daily' = c('daily', 'hourly', 'data.table'))

  out <- list(
    baseline = structure(dat[period == 'baseline', ], class = classV[[interval]]),
    install = structure(dat[period == 'install', ], class = classV[[interval]]),
    performance = structure(dat[period == 'performance', ], class = classV[[interval]]),
    property = unique(meterDT[meterID == meter, propertyName]),
    model = getDays(dat[period == 'baseline', date]) >= base.min &
      getDays(dat[period == 'performance', date]) >= perf.min,
    ivars = setdiff(names(dat), c('meterID', 'date', 'use', 'period', 'tbin', 'mm')),
    tcuts = tcuts
  )
}

#' Baseline Model
#' @import data.table
#' @import mlr
#' @export
ebModel <- function(dataList, method = c('gboost', 'regress'), mOptions = NULL){
  method <- match.arg(method)
  if(method == 'regress'){
    modelList <- lapply(dataList$meterDict, function(meter){
      regress(dat = dataList[['baseline']][[meter]], tcuts = dataList[['tcuts']][[meter]])
    })
  }
  if(method == 'gboost'){
    pSet <- list(max_depth = 3,
                 nrounds = seq(200, 1400, 200),
                 early_stopping_rounds = 5,
                 eta = 0.1,
                 blocks = 2,
                 cpus = 4)
    pSet <- c(pSet[setdiff(names(pSet), names(mOptions))], mOptions)

    parallelMap::parallelStart(mode = 'socket', cpus = pSet$cpus, level = 'mlr.tuneParams')
    suppressWarnings({
      suppressMessages(
        modelList <- lapply(dataList$meterDict, function(meter){
          gboost(dat = dataList[['baseline']][[meter]],
                 ivars = dataList[['ivars']][[meter]],
                 pSet = pSet)
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
ebPredict <- function(modelList, dataList, periods = c('baseline', 'install', 'performance')){
  out <- lapply(dataList$meterDict, function(meter){
    rbindlist(
      lapply(periods,
             function(period) predict(mod = modelList[[meter]],
                                      dat = dataList[[period]][[meter]],
                                      ivars = dataList[['ivars']][[meter]]))
    )[, .(meterID, date, period, use, pUse)]
  })
  out
}

#' Baseline Metrics Summary
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
#' Saving Summary
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
ebSummary <- function(predictions, propertyDict){
  metricsList <- lapply(predictions, function(x){
    ebBaselineSummary(x)
  })
  savingList <- lapply(predictions, function(x){
    ebSavingSummary(x)
  })

  propDT <- rbindlist(
    lapply(predictions, function(dat){
      dat <- copy(dat)
      dat[, propertyName:= propertyDict[meterID]]
      dat
    })
  )
  propMetrics <- rbindlist(
    lapply(unique(propertyDict), function(prop){
      dat <- ebBaselineSummary(propDT[propertyName == prop, ])
      dat[, meterID:= NULL]
      dat[, propertyName:= prop]
      unique(dat)
    })
  )
  propSavings <- rbindlist(
    lapply(unique(propertyDict), function(prop){
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

#' Modify formatted data
#' @import data.table
#' @export
ebModifyData <- function(dataList,
                         FUN,
                         meterSubset = NULL,
                         periods = c('baseline', 'install', 'performance')){
  dataList <- copy(dataList)
  if(is.null(meterSubset)) meterSubset <- dataList$meterDict
  for(period in periods){
    dataList[[period]][meterSubset] <- lapply(dataList[[period]][meterSubset],
                                              function (dat) structure(FUN(dat), class = class(dat)))
  }
  dataList[['ivars']] <- lapply(dataList$meterDict, function(meter){
    ivars = setdiff(names(dataList[['baseline']][[meter]]), c('meterID', 'date', 'use', 'period', 'tbin', 'mm'))
  })
  dataList
}

#' Calculate Loadpath Distance Matrix
#' @import TSclust
#' @import data.table
#' @export
ebDissCalc <- function(dat, method = 'DTWARP'){
  dat <- dcast(dat, yday(date) ~ hour(date), value.var = 'use')
  dat <- na.omit(dat)
  dayV <- dat$date
  dat <- as.matrix(dat[, -c('date')])
  rownames(dat) <- dayV
  diss(dat, method)
}

#' Cluster Data
#' @export
ebCluster <- function(dissM, k = 6){
  clustDayDict <- cutree(hclust(dissM), k = k)
  clustDict <- setNames(as.character(1:k), as.character(1:k))
  distances <- vapply(clustDict, function(clust){
    inClust <- names(clustDayDict[clustDayDict == clust])
    outClust <- names(clustDayDict[clustDayDict != clust])
    mean(as.matrix(dissM)[inClust, outClust])
  }, FUN.VALUE = numeric(1))
  count <- c(table(clustDayDict))
  list(clustDayDict = clustDayDict,
       distances = distances,
       count = count,
       stdev = sd(dissM),
       avg = mean(dissM))
}

#' Clustered Outlier Plot
#' @import data.table
#' @import dygraphs
#' @export
ebOutlierPlot <- function(dissM, dat, clustObj, countMax = 65, dstMin = 3){
  dat <- copy(as.data.table(dat))[, .(date, use)]
  dat[, cluster:= clustObj$clustDayDict[yday(date)]]
  dat[, ccount:= clustObj$count[cluster]]
  dat[, dst:= (clustObj$distances[cluster] - clustObj$avg) / clustObj$stdev]
  dat[, outlier:= ccount < countMax & dst > dstMin]
  dat[outlier == TRUE, `Clustered Outlier`:= use]
  dat[outlier == FALSE, `Normal`:= use]
  dygraph(dat[, .(date, `Clustered Outlier`, `Normal`)]) %>%
    dyBarSeries('Clustered Outlier', color = 'red') %>%
    dyBarSeries('Normal', color = 'gray')
}
