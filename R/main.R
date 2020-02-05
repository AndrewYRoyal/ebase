#' Data Format
#' @import data.table
#' @import parallelMap
#' @export
ebDataFormat <- function(
  x,
  install_dates = NULL,
  sites = NULL,
  norm_data = NULL,
  data_options = NULL,
  temp_bins = NULL,
  cpus = 1) {
  defaults <- list(interval = 'hourly',
                   base_length = 365,
                   perf_length = 365,
                   blackout = 0,
                   base_min = 0,
                   perf_min = 0,
                   tbin_type = 'simple',
                   occupancy_lookup = FALSE)
  data_options <- c(defaults[setdiff(names(defaults), names(data_options))], data_options)
  if(!(data_options$interval %in% c('hourly', 'daily'))) stop('Improper interval')
  meterDict <- setNames(unique(x$meterID), unique(x$meterID))
  if(is.null(sites)) sites <- meterDict
  cat(length(meterDict), 'total meters \n')
  no_tbin <- setdiff(meterDict, names(temp_bins))
  data_options$tbin_type = match.arg(data_options$tbin_type, c('simple', 'detailed', 'none'))
  temp_bins <- c(setNames(rep(10, length(no_tbin)), no_tbin), temp_bins)

  applyCall <- quote(f(meterDict,  function(m) {
    ebMeterFormat(dat = x[meterID == m, ],
                  inDate = install_dates[[m]],
                  ntbin = temp_bins[m],
                  data_options = data_options)
    }))
  applyCall[[1]] <- as.name('lapply')
  if(cpus > 1){
    parallelStartSocket(cpus)
    parallelLibrary('ebase')
    applyCall[[1]] <- as.name('parallelLapply')
  }
  dataList <- eval(applyCall)
  if(cpus > 1) parallelStop(); dataList <- setNames(dataList, meterDict)

  dataList <- dataList[sapply(dataList, function(x) !is.null(names(x)))]
  meterDict = setNames(names(dataList), names(dataList))
  cat(length(meterDict), 'with sufficient data \n')
  sites <- sites[names(sites) %in% meterDict]
  periods = names(dataList[[1]])
  names(periods) = periods
  list(
    stack = function(level = 'period') dstack(dataList, periods, level),
    list = function(level = 'period') dlist(dataList, periods, level),
    norms = norm_data,
    meterDict = meterDict,
    siteDict = sites,
    periods = periods)

}

#' Data Stack Method
#' @import data.table
#' @export
dstack = function(dataList, periods, level) {
  if(level == 'meter') {
    lapply(dataList, rbindlist)
  } else if(level == 'period') {
    lapply(periods, function(period) {
      meterDict = setNames(names(dataList), names(dataList))
      rbindlist(lapply(meterDict, function(meter) dataList[[meter]][[period]]))
    })
  }
}

#' Data List Method
#' @import data.table
#' @export
dlist = function(dataList, periods, level) {
  if(level == 'meter') {
    dataList
  } else if(level == 'period') {
    meterDict = setNames(names(dataList), names(dataList))
    lapply(periods, function(period) {
      lapply(meterDict, function(meter) dataList[[meter]][[period]])
    })
  }
}

#' Meter Format
#' @import data.table
#' @export
ebMeterFormat <- function(dat, inDate, ntbin, data_options) {
  sec_day = 60 * 60 * 24
  dat[, period:= 1]
  if(!is.null(inDate)) {
    dat[, period:=
          (date >= min(inDate) - (data_options$blackout / 2 + data_options$base_length) * sec_day) +
          (date >= min(inDate) - (data_options$blackout / 2) * sec_day) +
          (date >= max(inDate) + (data_options$blackout / 2) * sec_day) +
          (date >= max(inDate) + (data_options$blackout / 2 + data_options$perf_length) * sec_day)]
  }
  dat <- dat[period > 0 & period < 4]
  pNames <- c('baseline', 'blackout', 'performance')
  dat[, period:= as.factor(pNames[period])]
  towDict <- get_towDict(data_options$interval)
  if(data_options$interval == 'hourly') {
    dat[, tow:= towDict[paste0(weekdays(date), hour(date))]]
  } else if(data_options$interval == 'daily'){
    dat[, tow:= towDict[paste0(weekdays(date))]]
  }

  dat[, month:= month(date)]
  dat[, mm:= as.factor(month)]
  tcuts <- c(-Inf, quantile(dat$temp, 1:ntbin / ntbin))
  tcuts <- c(tcuts[-length(tcuts)], Inf)
  if(data_options$tbin_type != 'none') {
    dat <- get_tbins(dat, tcuts, data_options$tbin_type)
  }
  if(data_options$occupancy_lookup) {
    ocDT = ebOccupancy(dat[period == 'baseline'])
    dat = merge(dat, ocDT, by = c('tow', 'month'))
  }
  classStr <- list('hourly' = c('hourly'), 'daily' = c('daily', 'hourly'))[[data_options$interval]]
  setattr(dat, "class", c(classStr, class(dat)))

  if(uniqueN(dat[period == 'baseline', as.Date(date)]) < data_options$base_min |
     uniqueN(dat[period == 'performance', as.Date(date)]) < data_options$perf_min) return(NA)
  list(baseline = dat[period == 'baseline', ],
       blackout = dat[period == 'blackout', ],
       performance = dat[period == 'performance', ])
}

#' Baseline Model
#' @import data.table
#' @import mlr
#' @import parallelMap
#' @export
ebModel <- function(dat, method = c('regress', 'gboost', 'rforest', 'caltrack', 'hybrid'), model_options = NULL)
{
  method <- match.arg(method)
  model_defaults <- list(max_depth = 3,
                         nrounds = seq(200, 1400, 200),
                         ntree = 1:5 * 50,
                         early_stopping_rounds = 200,
                         eta = 0.1,
                         blocks = 2,
                         block_on_week = FALSE,
                         weights = NULL,
                         occupancy_lookup = FALSE,
                         ivars = NULL,
                         custom_lm = NULL)
  model_options <- c(model_defaults[setdiff(names(model_defaults), names(model_options))], model_options)
  modelCall <- quote(f(dat = dat, model_options = model_options))
  modelCall[[1]] <- as.name(method)
  cat('X')
  eval(modelCall)
}



#' Predictions
#' @import data.table
#' @import mlr
#' @export
ebPredict = function(modelList, dataList) {
  meterDict = dataList$meterDict
  siteDict = dataList$siteDict
  periods = dataList$periods
  norms = dataList$norms
  dataList = dataList$stack('meter')
  dataList = lapply(meterDict, function(meter) predict(modelList[[meter]], dataList[[meter]]))
  dataList = lapply(dataList, split, by = 'period')
  norm_predict = NULL
  if(!is.null(norms)) {
    norm_dat = lapply(meterDict, function(meter) ebForecast(modelList[[meter]], norms[[meter]]))
    norm_predict = function(modelList) {
      lapply(meterDict, function(meter) {
        merge(ebForecast(modelList[[meter]], norms[[meter]])[, .(meterID = meter, date, use)],
              norm_dat[[meter]][, .(date, pUse = use)])
      })
    }
  }
  out = list(
    stack = function(level = 'period') dstack(dataList, periods, level),
    list = function(level = 'period') dlist(dataList, periods, level),
    site_dat = data.table(site = as.factor(siteDict), meterID = names(siteDict)),
    norm_predict = norm_predict)
  setattr(out, 'class', c('prediction', class(out)))
  out
}

#' Summarize Prediction Data
#' @import data.table
#' @export
ebSummary <- function(x, ...) UseMethod('ebSummary')


#' Prediction and Savings Summary
#' @import data.table
#' @export
ebSummary.prediction = function(x, norm_modelList = NULL) {
  meter_baseline = merge(x$site_dat,
                         x$stack()[['baseline']],
                         by = 'meterID')
  site_baseline = meter_baseline[, lapply(.SD, sum),
                                 by = .(site, date),
                                 .SDcols = c('use', 'pUse')]
  meter_perf = merge(x$site_dat,
                     x$stack()[['performance']])
  site_perf = meter_perf[, lapply(.SD, sum),
                         by = .(site, date),
                         .SDcols = c('use', 'pUse')]
  meter_metrics <- meter_baseline[, list(r2 = R2(use, pUse),
                                         cvrmse = CVRMSE(use, pUse),
                                         nmbe = NMBE(use, pUse),
                                         baseline = sum(use)),
                                  by = .(meterID)]
  meter_savings <- meter_perf[, list(gross = sum(pUse - use),
                                     var_gross = varCalc(use, pUse)),
                              by = .(meterID)]
  site_metrics <- site_baseline[, list(r2 = R2(use, pUse),
                                        cvrmse = CVRMSE(use, pUse),
                                        nmbe = NMBE(use, pUse),
                                        baseline = sum(use)),
                                 by = .(site)]
  site_savings <- site_perf[, list(gross = sum(pUse - use),
                                   var_gross = varCalc(use, pUse)),
                            by = .(site)]
  norms = list(meters = NULL, sites = NULL)
  if(!is.null(norm_modelList)) {
    norm_dat = rbindlist(x$norm_predict(norm_modelList))
    norm_dat = merge(x$site_dat, norm_dat)
    setattr(norm_dat, 'class', c('norm', class(norm_dat)))
    norms = ebSummary(norm_dat)
  }
  return(list(meters = list(metrics = meter_metrics,
                            savings = meter_savings,
                            norms = norms$meters),
              sites = list(metrics = site_metrics,
                           savings = site_savings,
                           norms = norms$sites)))
}

#' Normalized Savings Summary
#' @import data.table
#' @export
ebSummary.norm = function(x) {
  meter_perf = x
  site_perf = meter_perf[, lapply(.SD, sum),
                         by = .(site, date),
                         .SDcols = c('use', 'pUse')]
  meter_savings <- meter_perf[, list(gross = sum(pUse - use),
                                     var_gross = varCalc(use, pUse)),
                              by = .(meterID)]
  site_savings <- site_perf[, list(gross = sum(pUse - use),
                                   var_gross = varCalc(use, pUse)),
                            by = .(site)]
  list(meters = meter_savings,
       sites = site_savings)
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

#' Apply Outlier Weights
#' @import data.table
#' @export
ebOutlierWeights <- function(dat)
{
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

#' Add Event
#' @import data.table
#' @export
ebAddEvent <- function(dataList, meter, nre_dates, event_name = 'nre')
{
  nre_dates = as.POSIXct(nre_dates, tz = 'UTC')
  for(period in c('baseline', 'blackout', 'performance')){
    dat = copy(dataList[[period]][[meter]])
    dat[, nre:= as.numeric(date >= min(nre_dates) & date <= max(nre_dates))]
    setnames(dat, 'nre', event_name)
    dataList[[period]][[meter]] = dat
  }
  dataList
}

#' Create Occupancy Lookup
#' @import data.table
#' @export
ebOccupancy <- function(dat)
{
  mmDict <- setNames(as.character(unique(dat$month)), as.character(unique(dat$month)))
  rbindlist(
    lapply(mmDict, function(segment){
      m_upper <- as.character(as.numeric(segment) + 1)
      m_lower <- as.character(as.numeric(segment) - 1)
      if(!(m_upper %in% mmDict)) m_upper <- as.character(min(as.numeric(mmDict)))
      if(!(m_lower %in% mmDict)) m_lower <- as.character(max(as.numeric(mmDict)))
      weightDict <- setNames(c(0, 1, 0), c(m_lower, segment, m_upper))

      out = dat[, .(date,
                    use,
                    tow,
                    cdh = as.numeric(temp > 65) * (temp - 65),
                    hdh = as.numeric(temp < 50) * (50 - temp),
                    month,
                    weight = weightDict[as.character(month)])]
      out[is.na(weight), weight:= 0]
      out[, res:= lm('use ~ cdh + hdh', data = out, weights = weight)$residuals]
      out = out[as.character(month) == segment,
                .(month = as.numeric(segment),
                  occupied = mean(as.numeric(res > 0)) > 0.65),
                by = .(tow)]
      out
    })
  )
}

#' Plot Meter Data
#' @import data.table
#' @export
ebPlot <- function(x, ...) UseMethod('ebPlot')

#' Plot Predictions
#' @import data.table
#' @import dygraphs
#' @export
ebPlot.data.table <- function(x, compress = TRUE) {
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
  dygraph(dat[, .(date, Actual, Predicted)], ylab = 'kWh') %>%
    dySeries("Actual", stepPlot = TRUE, fillGraph = TRUE, color = 'black') %>%
    dySeries("Predicted", strokeWidth = 1, stepPlot = TRUE, color = '#4889ce') %>%
    dyShading(from = datesV[3], to = datesV[4], color = "#CCEBD6") %>%
    dyEvent(datesV[2]) %>%
    dyEvent(datesV[3], 'Install Period', labelLoc = "top") %>%
    dyLegend(width = 400)
}


#' Reduce size of linear model
#' @import data.table
#' @export
strip_lm = function(x)
{
  x$data <- NULL
  x$y <- NULL
  x$linear.predictors <- NULL
  x$weights <- NULL
  x$fitted.values <- NULL
  x$model <- NULL
  x$prior.weights <- NULL
  x$residuals <- NULL
  x$effects <- NULL
  x$qr$qr <- NULL
  attr(x$terms,".Environment") = c()
  attr(x$formula,".Environment") = c()
  x
}

#' Forecast Method
#' @import data.table
#' @export
ebForecast <- function(model, ...) UseMethod('ebForecast')

#' Temp Bins Helper Function
#' @import data.table
#' @export
get_tbins <- function(dat, tcuts, tbin_type){
  ntbin <- length(tcuts) - 1
  dat[, tbin:= as.factor(cut(temp, tcuts))]
  if(tbin_type == 'detailed'){
    tcuts <- unname(c(0, tcuts[-1]))
    qtempList = lapply(1:ntbin, function(x) c(min = tcuts[x], max = tcuts[x + 1] - tcuts[x]))
    dat[, paste0('tbin_', 1:ntbin):=
          lapply(qtempList, function(q) pmin(as.numeric(temp > q['min']) * (temp - q['min']), q['max']))]
  }
  dat
}

#' TOW Dictionary Helper
#' @import data.table
#' @export
get_towDict <- function(x)
{
  if(x == 'hourly') {
    dict = setNames(
      1:168,
      paste0(
        rep(c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday', 'Sunday'),
            each =  24),
        rep(0:23, 7)))
  } else if(x == 'daily') {
    dict = setNames(1:7, c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday', 'Sunday'))
  }
  dict
}



