#' Baseline Model
#' @import data.table
#' @import lfe
#' @import stats
#' @import xgboost
#' @export
baselineModel <- function(dt, id.var, use.var, date.var, hour.var, temp.var, install.date.var,
                          nra.start.var = NULL, nra.end.var = NULL, base.length = NULL,
                          date.format = NULL, yoy.adjust = NULL, include.gboost = NULL){
  fCall <- match.call(expand.dots = FALSE)
  varV <- as.character(
    unlist(fCall[c('id.var', 'use.var', 'date.var', 'temp.var', 'hour.var', 'install.date.var')]))
  msngV <- varV[!(varV %in% names(dt))]
  if(length(msngV) > 0) stop(paste0('Missing parameter(s):', paste0(msngV, collapse = ', ')))
  if(!('base.length' %in% names(fCall))) fCall$base.length <- 12
  if(!('date.format' %in% names(fCall))) fCall$date.format <- '%m/%d/%Y'
  if(!('yoy.adjust' %in% names(fCall))) yoy.adjust <- FALSE
  if(!('include.gboost' %in% names(fCall))) include.gboost <- FALSE
  nra.est <- 'nra.start.var' %in% names(fCall)
  if(!nra.est & 'nra.end.var' %in% names(fCall)) stop(paste0('nra.start.date required.'))

  fCall <- fCall[setdiff(names(fCall), c("yoy.adjust", "include.gboost"))]
  fCall[[1L]] <- quote(formatData)
  dt <- eval(fCall)

  meterV <- unique(dt[, meterID])
  names(meterV) <- meterV

  panelRegList <- panelReg(dt = dt, meterV = meterV)

  rPredDT <- rbindlist(
    lapply(meterV,
           function(x) rPredict(
             meter = x, dt = dt, base.length = fCall$base.length, nra.est = nra.est,
             yoy.adjust = yoy.adjust)))
  dt <- merge(
    dt,
    rPredDT,
    by = c('meterID', 'date', 'hr'), all.x = TRUE)

  if(include.gboost){
    mlPredDT <- rbindlist(
      lapply(meterV,
             function(x) mlPredict(
               meter = x, dt = dt, nra.est = nra.est)))
    dt <- merge(
      dt,
      mlPredDT,
      by = c('meterID', 'date', 'hr'),
      all.x = TRUE)
  }

  keepV <- c('meterID', 'elct', 'pElct', 'MLpElct', 'date', 'hr', 'yr', 'yday', 'mm', 'dow',
             'baseline', 'postECM', 'nra')
  keepV <- keepV[keepV %in% names(dt)]
  dt <- dt[, c(keepV), with = FALSE][order(meterID, date)]
  out <- list(predictions = dt,
              modelFit = panelRegList$modelFit,
              coefTable = panelRegList$coefTable)

  out <- structure(out, class = 'baseline')
  return(out)
}

#' Plot Results
#' @import data.table
#' @import stats
#' @export
plot.baseline <- function(x, type = 'dailyload', ...){
  x <- structure(x, class = type)
  argumentV <- list(...)
  if(!('oHours' %in% names(argumentV))) oHours <- NA
  if(!('propertyTable' %in% names(argumentV))) propertyTable <- NA
  if(!('meterTable' %in% names(argumentV))) meterTable <- NA
  if(!('meterLevel' %in% names(argumentV))) meterLevel <- FALSE
  if(!('properties' %in% names(argumentV))) properties <- NA
  plot(x,
       oHours = argumentV$oHours,
       propertyTable = argumentV$propertyTable,
       meterTable = argumentV$meterTable,
       meterLevel = argumentV$meterLevel,
       properties = argumentV$properties)
}

#' Daily Load Profile
#' @import data.table
#' @import stats
#' @export

plot.dailyload <- function(x, ...){
  predDT <- x$predictions
  meterV <- unique(predDT[, meterID]); names(meterV) <- meterV
  predDT <- predDT[, list(Actual = sum(elct), Predicted = sum(pElct)), by = .(meterID, date, postECM)]
  predDT <- melt(predDT, id.vars = c('meterID', 'date', 'postECM'), variable.name = 'type', value.name = 'use')
  predDT <- predDT[, .SD, by = .(year(date), month(date), mday(date), date = date)]
  predDT[, c('year', 'type'):= lapply(list(year, type), as.factor)]

  plotList <- lapply(meterV, function(r){
    plotDT <- predDT[meterID == r, ]
    breaksV <- as.Date(seq(min(plotDT[, date], na.rm = TRUE), max(plotDT[, date], na.rm = TRUE), by = 'month'))
    labelsV <- month.abb[month(breaksV)]
    plotDT[, date:= as.Date(date)]

    ggplot(data = plotDT[meterID == r, ], aes(x = date, y = use, color = year, linetype = type)) +
      theme_light(base_size = 12) +
      theme(axis.text = element_text(angle = 40), legend.position = 'bottom',
            legend.text = element_text(size = 12)) +
      geom_line() +
      scale_color_manual('Year', values = c('steelblue1', 'royalblue1', 'royalblue4')) +
      scale_linetype_manual('', values = c(1, 3)) +
      scale_y_continuous('Daily kWh') +
      scale_x_date('Date', breaks = breaksV, labels = labelsV) +
      geom_vline(aes(xintercept = as.numeric(date), color = year), linetype = 2,
                 data = plotDT[postECM == 0, ][which.max(date)]) +
      ggtitle('Actual vs. Predicted Consumption', paste0('Meter ', r))
  })
  return(plotList)
}

#' Daily Savings
#' @import data.table
#' @import stats
#' @export

plot.dailysave <- function(x, propertyTable, meterTable, meterLevel = FALSE, ...){
  predDT <- merge(
    x$predictions[, lapply(list(elct = elct, pElct = pElct), sum), by = .(meterID, date)],
    unique(propertyTable[, .(meterID, propertyName)]),
    by = 'meterID')[order(propertyName, meterID, date)]
  predDT[, `Cumulative Savings`:= cumsum(pElct - elct), by = 'meterID']
  propertyV <- unique(predDT[, propertyName]); names(propertyV) <- propertyV
  out <- lapply(
    propertyV,
    function(p){
      inputDT <- predDT[propertyName == p, ]
      breaksV <- as.Date(seq(min(inputDT[, date]), max(inputDT[, date]), by = '2 month'))
      labelsV <- paste0(month.abb[month(breaksV)], '-', year(breaksV))
      inputDT[, Date:= as.Date(date)]
      xline <- as.Date(min(meterTable[propertyName == p, inDate]))
      yline <- unique(propertyTable[propertyName == p, deemedSavings])
      if(meterLevel){
        out <- ggplot(data = inputDT, aes(x = Date, y = `Cumulative Savings`, size = meterID)) +
          theme_light(base_size = 12) +
          theme(axis.text = element_text(angle = 40)) +
          geom_line(color = 'green2') +
          scale_x_date('Date', breaks = breaksV, labels = labelsV) +
          scale_y_continuous('Cumulative kWh Savings') +
          scale_size_manual(values = rep(1.5, 50)) +
          geom_vline(xintercept = as.numeric(xline), linetype = 2) +
          geom_hline(yintercept = yline, linetype = 2) +
          guides(size = FALSE)
      } else{
        out <- ggplot(data = inputDT, aes(x = Date, y = `Cumulative Savings`)) +
          theme_light(base_size = 12) +
          theme(axis.text = element_text(angle = 40)) +
          stat_summary(fun.y = sum, geom = "line", colour = "green4", size = 1.5) +
          scale_x_date('Date', breaks = breaksV, labels = labelsV) +
          scale_y_continuous('Cumulative kWh Savings') +
          guides(size = FALSE) +
          geom_vline(xintercept = as.numeric(xline), linetype = 2) +
          geom_hline(yintercept = yline, linetype = 2)
      }
    })
  return(out)
}


#' Hourly Savings
#' @import data.table
#' @import stats
#' @export

plot.hourlysave <- function(x, oHours, properties, meterTable, ...){
  plotDT <- merge(
    x$predictions,
    meterTable[, meterID, propertyName],
    by = 'meterID')
  if(length(properties) > 0) plotDT <- plotDT[propertyName %in% properties, ]

  plotDT[, meterTotal:= uniqueN(meterID), by = .(propertyName)]
  plotDT[, meterHour:= uniqueN(meterID), by = .(propertyName, date, hr)]
  plotDT <- plotDT[meterTotal == meterHour, ]
  plotDT <- plotDT[,
                   lapply(
                     list(elct = sum(elct), pElct = sum(pElct)),
                     sum),
                   by = .(hr, date, propertyName, postECM)]
  plotDT <- plotDT[,
                   lapply(
                     list(`Electricity` = elct, `Savings` = pElct - elct),
                     mean),
                   by = .(`Property` = propertyName, Hour = hr,
                          Period = factor(postECM, labels = c("Baseline", "Performance")))]
  out <- ggplot(data = plotDT[Period == 'Performance', ], aes(x = Hour, y = Savings, size = Property)) +
    theme_gdocs() +
    geom_line(alpha = 0.5, color = 'green4') +
    scale_size_manual(values = rep(1.5, 50)) +
    geom_vline(xintercept = oHours[1], linetype = 2) +
    geom_vline(xintercept = oHours[2], linetype = 2)
  return(out)

}



#' Model Summary Table
#' @import data.table
#' @import stats
#' @export
summary.baseline <- function(x){
  modelDT <- x$predictions[baseline == 1, ]
  uFunList <- get_uFunList(dt = modelDT)
  gBoostI <- 'MLpElct' %in% names(modelDT)

  if(gBoostI){
    modelDT <- modelDT[, list(RMSE_R = round(sqrt(mean((elct - pElct)^2)), 1),
                              RMSE_GB = round(sqrt(mean((elct - MLpElct)^2)), 1),
                              CVRMSE_R = round(100*sqrt(mean((elct - pElct)^2))/mean(elct), 0),
                              CVRMSE_GB = round(100*sqrt(mean((elct - MLpElct)^2))/mean(elct), 0),
                              NMBE_R = format(sum(pElct - elct)/sum(elct), digits = 1),
                              NMBE_GB = format(sum(MLpElct - elct)/sum(elct), digits = 1),
                              `Baseline Days` = round(.N/24, 0)),
                       by = 'meterID']
  } else{
    modelDT <- modelDT[, list(RMSE = round(sqrt(mean((elct - pElct)^2)), 1),
                              CVRMSE = round(100*sqrt(mean((elct - pElct)^2))/mean(elct), 0),
                              NMBE = format(sum(pElct - elct)/sum(elct), digits = 1),
                              baseDays = round(.N/24, 0)),
                       by = 'meterID']
  }
  modelDT[, R_Sq:= round(x$modelFit[meterID], 2)]
  modelDT <- merge(
    modelDT,
    x$predictions[baseline == 0, list(Savings = sum(pElct - elct)), by = 'meterID'],
    by = 'meterID')

  # for(r in names(uFunList)) modelDT[meterID == r, Frac_U:= round(100*uFunList[[r]](m = baseDays*24, t = 1, f = 0.05), 0)]
  # modelDT <- merge(modelDT, x$trend[variable == 'est', .(meterID, YoY = round(100*value))],
  #                  by = 'meterID')
  # if(is.data.table(x$NRAdjust)){
  #   modelDT <- merge(modelDT, x$NRAdjust[variable == 'est', .(meterID, NRA = round(100*value))],
  #                    by = 'meterID', all.x = TRUE)
  # }
  return(modelDT)
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


# Deprecated
#===========================================================

# Move all thermal stuff to the thermal plot method
# thermalDT <- rbindlist(lapply(pDTList, function(x) x[grep('tbin', coef)]))
# thermalDT[, coef:= vapply(coef,
#                           function(x) regmatches(x = x, regexpr('(?<=bin).*', x, perl = TRUE)),
#                           FUN.VALUE = character(1))]
# thermalDT <- thermalDT[order(meterID, variable, as.numeric(coef))]
#
# thermalDT[, coef:= tbinV[as.numeric(coef)]]

# Thermal Sensitivity
# print_thermalSensitivity <- function(dt, export.path = NULL, print = FALSE){
#   dt <- copy(dt)
#   tbinV <- c('<50', seq(55, 70, 5), seq(80, 95, 5), '>95')
#   dt[, xVar:= sapply(tbinV, function(x) min(grep(x, tbinV)))]
#   meterV <- unique(dt[, meterID]) ; names(meterV) <- meterV
#   dt[, value:= value*100]
#
#   plotList <- lapply(meterV, function(r){
#     ggplot(data = dt[meterID == r, ],
#            aes(x = xVar, y = value, linetype = as.factor(variable), size = as.factor(variable))) +
#       theme_light(base_size = 12) +
#       theme(legend.position = 'none') +
#       geom_line() +
#       scale_y_continuous('Percentage Change') +
#       scale_x_continuous('Outside Temperature', breaks = 1:10, labels = tbinV) +
#       scale_linetype_manual('', values = c(1, 2, 2)) +
#       scale_size_manual('', values = c(1, .5, .5)) +
#       geom_hline(yintercept = 0, size = 1, linetype = 3) +
#       ggtitle('Change in Hourly kWh Due to Temperature', paste0('Meter ', r))
#   })
#   if(!is.null(export.path)){
#     for(r in meterV){
#       ggsave(plotList[[r]],
#              filename = paste0(export.path, '/tprofile_', grep(r, meterV), '.pdf'),
#              width = 10, height = 6)
#     }
#   }
#
#   if(print) return(plotList)
# }
