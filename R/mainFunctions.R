#' Baseline Model
#' @import data.table
#' @import lfe
#' @import stats
#' @import xgboost
#' @export

baselineModel <- function(
  useDT, meterDT, base.length = 12, date.format = '%m/%d/%Y', gboost = FALSE, padding = 0){
  uMsngV <- setdiff(
    c('meterID', 'elct', 'date', 'hr', 'temp'),
    names(useDT))
  mMsngV <- setdiff(
    c('meterID', 'inDate'),
    names(meterDT))
  if(length(uMsngV) > 0) stop(paste0('Missing parameter(s):', paste0(uMsngV, collapse = ', ')))
  if(length(mMsngV) > 0) stop(paste0('Missing parameter(s):', paste0(mMsngV, collapse = ', ')))

  out <- formatData(useDT = useDT,
                    meterDT = meterDT,
                    base.length = base.length,
                    date.format = date.format,
                    padding = padding)

  meterV <- unique(out[, meterID])
  names(meterV) <- meterV
  panelRegList <- panelReg(dt = out, meterV = meterV)

  nra.est <- 'nra' %in% names(out)
  rPredDT <- rbindlist(
    lapply(meterV,
           function(x) rPredict(
             meter = x, dt = out, nra.est = nra.est)))
  out <- merge(
    out,
    rPredDT,
    by = c('meterID', 'date', 'hr'), all.x = TRUE)

  if(gboost){
    mlPredDT <- rbindlist(
      lapply(meterV,
             function(x) mlPredict(
               meter = x, dt = out, nra.est = nra.est)))
    out <- merge(
      out,
      mlPredDT,
      by = c('meterID', 'date', 'hr'),
      all.x = TRUE)
  }

  out[, c('dHour', 'tbin'):= NULL]
  dropV <- grep('.*(?=\\d)', names(out), perl = TRUE, value = TRUE)
  out <- out[, setdiff(names(out), dropV), with = FALSE][order(meterID, date, hr)]
  out <- list(predictions = out,
              coefTable = panelRegList$coefTable,
              models = panelRegList$modelList,
              meterDT = meterDT)
  out <- structure(out, class = 'baseline')
  return(out)
}

#' Daily Load Profile
#' @import data.table
#' @import stats
#' @export

plot.baseline <- function(x, gboost = FALSE, ...){
  meterV <- unique(x$predictions[, meterID]); names(meterV) <- meterV
  inputDT <- x$predictions
  if(gboost) inputDT[, pElct:= MLpElct]
  inputDT <- inputDT[, list(
    Actual = sum(elct),
    Predicted = sum(pElct)),
    by = .(meterID, date, period)]

  inputDT <- melt(inputDT,
                  id.vars = c('meterID', 'date', 'period'),
                  variable.name = 'type', value.name = 'use')

  inputDT[, c('period', 'type'):= lapply(list(period, type), as.factor)]
  profileList <- lapply(meterV, function(r){
    plotDT <- inputDT[meterID == r, ]
    plotDT[, date:= as.Date(date)]

    ggplot(data = plotDT, aes(x = date, y = use, color = period, linetype = type)) +
      theme_light(base_size = 12) +
      theme(axis.text = element_text(angle = 40), legend.position = 'bottom',
            legend.text = element_text(size = 12)) +
      geom_line() +
      scale_color_manual('Period', values = c('black', 'gray45', 'royalblue')) +
      scale_linetype_manual('', values = c(1, 3)) +
      scale_y_continuous('Daily kWh') +
      scale_x_date('Date') +
      # scale_x_date('Date', breaks = breaksV, labels = labelsV) +
      geom_vline(aes(xintercept = as.numeric(date)), linetype = 2,
                 data = plotDT[period == 'install', ][which.max(date)]) +
      geom_vline(aes(xintercept = as.numeric(date)), linetype = 2,
                 data = plotDT[period == 'install', ][which.min(date)]) +
      ggtitle('Actual vs. Predicted Consumption', paste0('Meter ', r))
  })
  return(profileList)
}



#' Model Summary Table
#' @import data.table
#' @import stats
#' @export
summary.baseline <- function(x, oHours = NULL){
  meterDT <- copy(x$meterDT)
  predDT <- copy(x$predictions)

  if(!('MLpElct' %in% names(predDT))) predDT[, MLpElct:= NA]
  R2 <- function(actual, predicted) round(1 - sum((actual - predicted)^2)/
                                                sum((actual - mean(actual))^2), 2)
  CVRMSE <- function(actual, predicted) round(100*sqrt(mean((actual - predicted)^2))/
                                                mean(actual), 0)
  NMBE <- function(actual, predicted) format(100*sum(predicted - actual)/
                                               sum(actual), digits = 1)
  sumMeterDT <- merge(
    meterDT[, .(meterID)],
    predDT[period == 'baseline',
           list(
             R2_GB = R2(elct, MLpElct),
             R2_R = R2(elct, pElct),
             CVRMSE_GB = CVRMSE(elct, MLpElct),
             CVRMSE_R = CVRMSE(elct, pElct),
             NMBE_GB = NMBE(elct, MLpElct),
             NMBE_R = NMBE(elct, pElct),
             baselineDays = round(.N/24, 0),
             `Annual kWh` = sum(elct)),
           by = .(meterID)],
    by = 'meterID')
  sumMeterDT <- merge(
    sumMeterDT,
    predDT[period == 'performance',
           list(
             Savings_GB = sum(MLpElct - elct),
             Savings_R = sum(pElct - elct),
             performanceDays = round(.N/24, 0)),
           by = .(meterID)],
    by = 'meterID')
  sumPropertyDT <- NA
  if('propertyName' %in% names(meterDT)){
    sumPropertyDT <- merge(
      meterDT[, .(meterID, propertyName)],
      predDT,
      by = 'meterID')[period == 'baseline',
      lapply(
        list(elct = elct, pElct = pElct, MLpElct = MLpElct),
        sum),
      by = .(propertyName, date, hr)]
    sumPropertyDT <-
      sumPropertyDT[,
                    list(
                      R2_GB = R2(elct, MLpElct),
                      R2_R = R2(elct, pElct),
                      CVRMSE_GB = CVRMSE(elct, MLpElct),
                      CVRMSE_R = CVRMSE(elct, pElct),
                      NMBE_GB = NMBE(elct, MLpElct),
                      NMBE_R = NMBE(elct, pElct),
                      `Annual kWh` = sum(elct)),
                    by = .(propertyName)]
    sumPropertyDT <- merge(
      sumPropertyDT,
      merge(
        sumMeterDT[, .(meterID, Savings_R, Savings_GB)],
        meterDT[, .(propertyName, meterID)])[, list(Savings_GB = sum(Savings_GB),
                                                    Savings_R = sum(Savings_R)),
                                             by = .(propertyName)],
      by = 'propertyName')
  }

  return(list(
    summaryMeter = sumMeterDT,
    summaryProperty = sumPropertyDT))
}

#' Truncated Savings
#' @import data.table
#' @import stats
#' @export
truncated_savings <- function(x){
  if(length(setdiff(c('oStart', 'oEnd'), names(x$meterDT))) > 1) stop('No oStart or oEnd in meterDT.')

  opHour <- function(hr, start, end) hr %in% unique(c(seq(start, min(end, 23)), seq(0, end)))

  meterSave <- merge(
    x$meterDT[, .(meterID, propertyName, oStart, oEnd)],
    x$predictions[period == 'performance', ],
    by = 'meterID')
  meterSave[is.na(oStart), c('oStart', 'oEnd'):= list(0, 23)]
  meterSave[, operating:= mapply(opHour, hr = hr, start = oStart, end = oEnd)]
  if(!('MLpElct' %in% names(meterSave))) meterSave[, MLpElct:= NA]

  meterSave <- meterSave[operating == TRUE, list(
    TSavings_GB = sum(MLpElct - elct),
    TSavings_R = sum(pElct - elct)),
    by = .(meterID, propertyName)]
  propertySave <- meterSave[, lapply(
    list(TSavings_R = TSavings_R, TSavings_GB = TSavings_GB),
    sum),
    by = .(propertyName)]
  return(list(
    meter = meterSave[, .(meterID, TSavings_GB, TSavings_R)],
    property = propertySave))
}



#' Fractional Uncertainty Table
#' @import data.table
#' @import lfe
#' @import stats
#' @import ggplot2
#' @export
fUncertainty <- function(dt, fSavings, tstat = 1, export.path = NULL){
  meterV <- unique(dt[, meterID]); names(meterV) <- meterV
  uFunList <- get_uFunList(dt = dt)
  obs <- max(sapply(meterV, function(r) uniqueN(dt[meterID == r & baseline == 1, ]))) # Taking max # of observations across meters

  outDT <- data.table(m = rep(1:obs, times = length(fSavings)),
                      t = rep(tstat, times = obs*length(fSavings)),
                      f = rep(fSavings, each = obs))

  for(u in names(uFunList)) outDT[, paste0('', u):= mapply(uFunList[[u]], m, t, f, SIMPLIFY = TRUE)]
  outDT <- melt(outDT, id.vars = c('m', 't', 'f'), value.name = 'fUncertainty', variable.name = 'meterID')
  outDT[, f:= as.factor(f)]

  breaksV <- seq(30*24, 365*24, 30*24)
  labelsV <- as.character(breaksV/24)
  cPallete <- c('steelblue1', 'steelblue3', 'royalblue2', 'royalblue4', 'blue4')

  plotList <- lapply(meterV, function(r){
    ggplot(data  = outDT[inrange(m, 30*24, 365*24) & meterID == r, ], aes(x = m, y = fUncertainty, color = f)) +
      theme_light(base_size = 12) +
      theme(axis.text = element_text(angle = 40)) +
      geom_line(size = 1.5) +
      scale_y_continuous('Fractional Uncertainty', breaks = 1:10/10, labels = as.character(1:10/10)) +
      scale_x_continuous('Post-ECM Days', breaks = breaksV, labels = labelsV) +
      scale_color_manual('Savings', values = cPallete) +
      geom_hline(yintercept = 0.5, size = 1.5, linetype = 2) +
      ggtitle('Model Uncertainty', paste0('Meter ', r))
  })

  if(!is.null(export.path)){
    for(r in meterV){
      ggsave(plotList[[r]], filename = paste0(export.path, '/fU_', grep(r, meterV), '.pdf'),
             width = 10, height = 6)
    }
  }

  return(list(table = outDT,
              plots = plotList))
}


