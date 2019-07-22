#' Data Format
#' @import data.table
#' @export
ebDataFormat <- function(x,
                         install_dates,
                         sites = NULL,
                         data_options = NULL,
                         temp_bins = NULL){
  defaults <- list(interval = 'hourly',
                   base_length = 365,
                   perf_length = 365,
                   blackout = 0,
                   base_min = 0,
                   perf_min = 0)
  data_options <- c(defaults[setdiff(names(defaults), names(data_options))], data_options)
  if(!(data_options$interval %in% c('hourly', 'daily'))) stop('Improper interval')
  meterDict <- setNames(unique(x$meterID), unique(x$meterID))
  cat(length(meterDict), 'total meters \n')
  if(length(setdiff(meterDict, names(install_dates))) > 0) stop(
    sprintf('No install date for %s \n', paste(setdiff(meterDict, names(install_dates)), sep = ', ')))
  no_tbin <- setdiff(meterDict, names(temp_bins))
  temp_bins <- c(setNames(rep(10, length(no_tbin)), no_tbin), temp_bins)

  dataList <- lapply(
    meterDict,
    function(m){
      ebMeterFormat(dat = x[meterID == m, ],
                    inDate = install_dates[[m]],
                    ntbin = temp_bins[m],
                    data_options = data_options)
  })
  meterDict <- meterDict[sapply(meterDict, function(meter) dataList[[meter]][['model']])]
  cat(length(meterDict), 'with sufficient data \n')
  sites <- sites[names(sites) %in% meterDict]

  out <- list(
    pretrial = lapply(meterDict, function(meter) dataList[[meter]][['pretrial']]),
    baseline = lapply(meterDict, function(meter) dataList[[meter]][['baseline']]),
    install = lapply(meterDict, function(meter) dataList[[meter]][['install']]),
    performance = lapply(meterDict, function(meter) dataList[[meter]][['performance']]),
    meterDict = meterDict,
    siteDict = sites,
    ivars = lapply(meterDict, function(meter) dataList[[meter]][['ivars']]),
    tcuts = lapply(meterDict, function(meter) dataList[[meter]][['tcuts']]))
  out
}

#' Meter Format
#' @import data.table
#' @export
ebMeterFormat <- function(dat, inDate, ntbin, data_options){
  dat[, period:=
        (date >= min(inDate) - as.difftime(data_options$blackout / 2 + data_options$base_length + 30, units = 'days')) +
        (date >= min(inDate) - as.difftime(data_options$blackout / 2 + data_options$base_length, units = 'days')) +
        (date >= min(inDate) - as.difftime(data_options$blackout / 2, units = 'days')) +
        (date >= max(inDate) + as.difftime(data_options$blackout / 2, units = 'days')) +
        (date >= max(inDate) + as.difftime(data_options$blackout / 2 + data_options$perf_length, units = 'days'))]
  dat <- dat[period > 0 & period < 5, ]
  pNames <- c('pretrial', 'baseline', 'install', 'performance')
  dat[, period:= as.factor(pNames[period])]
  dat[, tow:= .GRP, by = .(wday(date), hour(date))]
  dat[, month:= month(date)]
  dat[, mm:= as.factor(month)]
  tcuts <- c(-Inf, quantile(dat$temp, 1:ntbin / ntbin))
  dat[, tbin:= as.factor(cut(temp, tcuts))]

  getDays <- function(x) uniqueN(as.POSIXct(round(x, 'days')))
  classStr <- list('hourly' = c('hourly'), 'daily' = c('daily', 'hourly'))[[data_options$interval]]
  setattr(dat, "class", c(classStr, class(dat)))

  out <- list(
    pretrial = dat[period == 'pretrial', ],
    baseline = dat[period == 'baseline', ],
    install = dat[period == 'install', ],
    performance = dat[period == 'performance', ],
    model = getDays(dat[period == 'baseline', date]) >= data_options$base_min &
      getDays(dat[period == 'performance', date]) >= data_options$perf_min,
    ivars = setdiff(names(dat), c('meterID', 'date', 'use', 'period', 'tbin', 'mm')),
    tcuts = tcuts
  )
}

#' Baseline Model
#' @import data.table
#' @import mlr
#' @export
ebModel <- function(dataList, method = c('regress', 'gboost', 'rforest'), model_options = NULL){
  method <- match.arg(method)
  pSet <- list(max_depth = 3,
               nrounds = seq(200, 1400, 200),
               ntree = 1:5 * 50,
               early_stopping_rounds = 5,
               eta = 0.1,
               blocks = 2,
               cpus = 4,
               weights = NULL)
  pSet <- c(pSet[setdiff(names(pSet), names(model_options))], model_options)
  if(method == 'regress'){
    modelList <- lapply(dataList$meterDict, function(meter){
      regress(dat = dataList[['baseline']][[meter]],
              pSet = pSet)
    })
  }
  if(method != 'regress'){
    modelCall <- quote(f(dat = dataList[['baseline']][[meter]],
                         ivars = dataList[['ivars']][[meter]],
                         pSet = pSet))
    modelCall[[1]] <- as.name(method)
    parallelMap::parallelStart(mode = 'socket', cpus = pSet$cpus, level = 'mlr.tuneParams')
    suppressWarnings({
      suppressMessages(
        modelList <- lapply(dataList$meterDict, function(meter){
          eval(modelCall)
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
ebPredict <- function(modelList, dataList, periods = c('pretrial', 'baseline', 'install', 'performance')){
  out <- lapply(dataList$meterDict, function(meter){
    rbindlist(
      lapply(periods,
             function(period) predict(mod = modelList[[meter]],
                                      dat = dataList[[period]][[meter]],
                                      ivars = dataList[['ivars']][[meter]])),
      fill = TRUE)[, .(meterID, date, period, use, pUse)]
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
ebSummary <- function(predictions, siteDict){
  metricsList <- lapply(predictions, function(x){
    ebBaselineSummary(x)
  })
  savingList <- lapply(predictions, function(x){
    ebSavingSummary(x)
  })

  siteDT <- rbindlist(
    lapply(predictions, function(dat){
      dat <- copy(dat)
      dat[, site:= siteDict[meterID]]
      dat
    })
  )
  siteMetrics <- rbindlist(
    lapply(unique(siteDict), function(s){
      dat <- ebBaselineSummary(siteDT[site == s, ])
      dat[, meterID:= NULL]
      dat[, site:= s]
      unique(dat)
    })
  )
  siteSavings <- rbindlist(
    lapply(unique(siteDict), function(s){
      dat <- ebSavingSummary(siteDT[site == s, ])
      dat[, meterID:= NULL]
      dat[, site:= s]
      unique(dat)
    })
  )
  return(list(Meters = list(metrics = rbindlist(metricsList),
                            savings = rbindlist(savingList)),
              Sites = list(metrics = siteMetrics,
                           savings = siteSavings)))
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
    dySeries("Actual", stepPlot = TRUE, fillGraph = TRUE, color = 'black') %>%
    dySeries("Predicted", strokeWidth = 1, stepPlot = TRUE, color = '#4889ce') %>%
    dyShading(from = datesV[3], to = datesV[4], color = "#CCEBD6") %>%
    dyEvent(datesV[2]) %>%
    dyEvent(datesV[3], 'Install Period', labelLoc = "top") %>%
    dyEvent(datesV[1], 'Pre-Trial', labelLoc = "top") %>%
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
  length(actual) * (sum((actual - mean(actual))^2)/adjustedN(actual, length(actual)) +
                      sum((predicted - mean(predicted))^2)/adjustedN(predicted, length(predicted)) -
                      2 * cov(actual, predicted))
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

#' Apply Outlier Weights
#' @import data.table
#' @export

ebOutlierWeights <- function(dat){
  dat <- copy(dat)
  weightFn <- function(z0, z1, p0, p1){
    b <- (log(1 / p1 - 1) - log(1 / p0 - 1)) / (z1 - z0)
    a <- log(1 / p0 - 1) - b * z0
    function(z) 1 / (1 + exp(b * z + a))
  }
  wt <- weightFn(1, 3, .75, .001)
  dat[, z:= abs((use - mean(use)) / sd(use)), by = .(tow)]
  dat[, weight:= wt(z)]
  dat[, z:= NULL]
}


# curve(wt, from = 0, to = 3)
