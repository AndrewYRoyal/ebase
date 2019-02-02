#' Format Data
#' @export
formatData <- function(dt, id.var, use.var, date.var, hour.var, temp.var, install.date.var,
                           nra.start.var, nra.end.var,
                           base.length, date.format){
  dt <- copy(dt)
  fCall <- match.call(expand.dots = FALSE)
  vnameSetV <- c('id.var', 'use.var', 'date.var', 'hour.var', 'temp.var', 'install.date.var', 'nra.start.var', 'nra.end.var')
  vnameOldV <- as.character(unlist(fCall[names(fCall) %in% vnameSetV]))
  names(vnameOldV) <- names(fCall[names(fCall) %in% vnameSetV])
  vnameKeyV <- c(id.var = 'meterID', use.var = 'elct', date.var = 'date', hour.var = 'hr',
                 temp.var = 'temp', install.date.var = 'inDate', nra.start.var = 'nraStart',
                  nra.end.var = 'nraEnd')
  vnameNewV <- vnameKeyV[names(vnameOldV)]
  setnames(dt, vnameOldV, vnameNewV)
  dt <- na.omit(dt, c('date', 'elct'))

  if(!('nraStart' %in% names(dt))) dt[, nraStart:= as.POSIXct(NA)]
  if(!('nraEnd' %in% names(dt))) dt[, nraEnd:= as.POSIXct(NA)]
  if(is.character(dt[, date])) dt[, date:= as.POSIXct(date, format = date.format, tz = 'UTC')]
  if(is.character(dt[, inDate])) dt[, inDate:= as.POSIXct(inDate, format = date.format, tz = 'UTC')]
  if(is.character(dt[, nraStart])) dt[, nraStart:= as.POSIXct(nraStart, format = date.format, tz = 'UTC')]
  if(is.character(dt[, nraEnd])) dt[, nraEnd:= as.POSIXct(nraEnd, format = date.format, tz = 'UTC')]

  mDatesDT <- unique(dt[, .(meterID, inDate, nraStart, nraEnd)])
  for(r in unique(mDatesDT[, meterID])){
    mDatesDT[meterID == r, blStartDate:= seq(inDate,
                                             length = 2,
                                             by = paste0("-", base.length, " months"))[2]]
    mDatesDT[meterID == r, yoyDate:= seq(inDate,
                                         length = 2,
                                         by = paste0("-", 1, " months"))[2]]
    mDatesDT[meterID == r, endDate:= seq(inDate,
                                         length = 2,
                                         by = paste0("+", 12, " months"))[2]]
  }
  dt <- merge(
    dt[, .(meterID, elct, date, hr, temp)],
    mDatesDT)

  maxDate <- max(dt[, date])
  dt[is.na(nraEnd) & !is.na(nraStart), nraEnd:= maxDate]
  dt[, baseline:= as.numeric(date >= blStartDate & date < inDate)]
  dt[, postECM:= as.numeric(date >= inDate & date < endDate)]
  dt[, nra:= as.numeric(date >= nraStart & date <= nraEnd)]
  dt[, yoy:= as.numeric(date >= yoyDate)]
  dt <- dt[baseline == 1 | postECM == 1, ]

  dt[, tbin:= pmin(ceiling(pmax(0, (temp - 50)/5)) + 1, 11)]
  for(t in unique(dt[, tbin])) dt[, paste0('tbin', t):= as.numeric(tbin == t)]
  dt[, c('mm', 'dow'):= list(month(date), wday(date))]
  for(m in unique(dt[, mm])) dt[, paste0('mm', m):= as.numeric(mm == m)]
  dt[, dHour:= .GRP, by = c('meterID', 'dow', 'hr')]
  dt[, c('inDate', 'blStartDate', 'yoyDate', 'nraStart', 'nraEnd'):= NULL]
  return(dt)
}

#' Panel Regression
#' @export
panelReg <- function(dt, meterV){
  tbinV <- c('<50', seq(55, 70, 5), '', seq(80, 95, 5), '>95')
  panelList <- lapply(meterV, function(x) panelModel(x, dt = dt))
  modelFitV <- vapply(meterV,
                         function(x) panelList[[x]]$r2, FUN.VALUE = numeric(1))
  coefTableList <- lapply(panelList, function(x) coefTable(x))
  coefTableList <- lapply(
    names(coefTableList),
    function(x){
      coefTableList[[x]][, meterID:= x]
      setcolorder(coefTableList[[x]], c('meterID', 'coef', 'variable', 'value', 'stde', 'tvalue'))
    })
  coefTableDT <- rbindlist(coefTableList)
  return(list(modelFit = modelFitV, coefTable = coefTableDT))
}

#' Panel Model
#' @export
panelModel <- function(m, dt){
  regDT <- dt[meterID == m, ]
  tbinV <- unique(regDT[, tbin])[-5] # set intercept on 5th bin
  yoyI <- uniqueN(regDT[baseline == 1, .(year(date), mm)]) > 12
  nraI <- 'nra' %in% names(regDT) && !anyNA(regDT[, nra])

  rForumula <- paste0('log(elct + .01)', '~',
                      paste0('tbin', tbinV, collapse = '+'),
                      ifelse(yoyI, '+ yoy', ''),
                      ifelse(nraI, ' + nra', ''),
                      ' | dHour + mm | 0 | 0')
  out <- do.call('felm', list(as.formula(rForumula), data = as.name('regDT')))
  out <- summary(out)
  return(out)
}

#' Coefficient Summary
#' @export
coefTable <- function(x){
  estM <- coef(x)
  estM <- cbind(coef = rownames(estM), estM)
  estDT <- as.data.table(estM[, c('coef', 'Estimate', 'Std. Error', 't value')])
  numV <- setdiff(names(estDT), c('coef', 'meterID'))
  estDT[, paste0('', numV):= lapply(numV, function(x) as.numeric(get(x)))]
  setnames(estDT, names(estDT), c('coef', 'est', 'stde', 'tvalue'))
  estDT[, est:=
          vapply(est,
                 function(x) get_pctTransform(x),
                 FUN.VALUE = numeric(1))]
  estDT[, stde:= mapply(get_pctTransform, est, stde)]
  estDT[, c('ub', 'lb'):= list(est + 1.96*stde, est - 1.96*stde)]
  estDT <- melt(estDT, id.vars = c('coef', 'tvalue', 'stde'))

  return(estDT)
}

#' Transform Coefficients to Percent
#' @export
get_pctTransform <- function(parameter, stde = NULL){
  parameter <- exp(parameter) - 1
  if(is.null(stde)) return(parameter)
  else return(sqrt(exp(2*parameter)*(exp(-stde^2) - exp(-2*stde^2))))
}

#' Regression Predictions
#' @export
rPredict <- function(meter, dt, nra.est, yoy.adjust, base.length){
  regDT <- dt[meterID == meter, ]
  mmV <- unique(regDT[, mm])[-1]
  tbinV <- unique(regDT[, tbin])[-5]

  rFormula <- paste0('elctDM ~',
                     paste0('mm', mmV, 'DM', collapse = '+'), '+',
                     paste0('tbin', tbinV, 'DM', collapse = '+'),
                     ifelse(base.length > 11 & yoy.adjust, '+ yoy', ''),
                     ifelse(nra.est && !anyNA(dt[, nra]), '+ nra', ''))
  varV <- as.list(c('elct', paste0('mm', mmV), paste0('tbin', tbinV), 'yoy'))
  names(varV) <- varV

  regDT[baseline == 1,
        paste0(varV, 'm'):=
          lapply(varV,
                 function(x) mean(get(x), na.rm = TRUE)),
        by = .(dHour)]
  regDT <- merge(
    regDT,
    unique(regDT[baseline == 1, c('dHour', paste0(varV, 'm')), with = FALSE]),
    by = 'dHour', suffixes = c('', '.pred'))
  regDT[, paste0(varV, 'DM'):=
          lapply(varV,
                 function(x) get(x) - get(paste0(x, 'm.pred'))),
     by = c('dHour', 'baseline', 'meterID')]

  est <- lm(rFormula, data = regDT[baseline == 1, ])
  regDT[, pElct:= elctm.pred + predict(est, regDT)]
  return(regDT[,.(meterID, date, hr, pElct)])
}
#' Gradient Boost Predictions
#' @export
mlPredict <- function(meter, dt, nra.est){
  regDT <- dt[meterID == meter, ]
  regDT <- regDT[, dHour:= as.factor(dHour)]
  mmV <- unique(regDT[, mm])[-1]

  keepV <- c('meterID', 'elct', 'baseline', 'date', 'hr', 'temp', 'dHour', paste0('mm', mmV))
  if(nra.est && !anyNA(regDT[, nra])) keepV <- c(keepV, 'nra')
  regDT <- regDT[, keepV, with = FALSE]

  baselineM <- xgb.DMatrix(
    data = sparse.model.matrix(elct ~ ., data = regDT[baseline == 1, -c('meterID', 'baseline', 'date', 'hr')]),
    label = regDT[baseline == 1, elct]
  )

  cb.cv.predict(save_models = TRUE)
  nIterate <- xgb.cv(data = baselineM,
                     nfold = 10, max_depth = 6, eta = .5, nthread = 8, nrounds = 2000,
                     early_stopping_rounds = 5, objective = 'reg:linear', verbose = 0)$best_iteration
  gbModel <- xgboost(data = baselineM, max_depth = 6, eta = .5, nthread = 8, nrounds = nIterate,
                     early_stopping_rounds = 5, objective = 'reg:linear', verbose = 0)
  regDT[baseline == 1, MLpElct:= predict(gbModel, baselineM)]

  if(uniqueN(regDT[baseline == 0, ]) > 30){
    postM <- xgb.DMatrix(
      data = sparse.model.matrix(elct ~ ., data = regDT[baseline == 0, -c('meterID', 'baseline', 'date', 'hr', 'MLpElct')]),
      label = regDT[baseline == 0, elct]
    )
    regDT[baseline == 0, MLpElct:= predict(gbModel, postM)]
  }

  return(regDT[, .(meterID, date, hr, MLpElct)])
}

#' Uncertainty Function List
#' @export
get_uFunList <- function(dt){
  meterV <- unique(dt[, meterID]); names(meterV) <- meterV
  p <- 190 # 7x24 + 12 + 10
  dt <- copy(dt[baseline == 1, ])[order(meterID, date, hr)]
  dt[, lagElct:= shift(elct, 1, type = 'lag'), by = 'meterID']
  dt[, sqError:= (elct - pElct)^2]

  rhoList <- lapply(meterV, function(x) cor(dt[!is.na(lagElct) & meterID == x, elct],
                                            dt[!is.na(lagElct) & meterID == x, lagElct]))

  nList <- lapply(meterV, function(x) uniqueN(dt[meterID == x, ]))
  nRhoList <- lapply(meterV, function(x) nList[[x]]*(1 - rhoList[[x]])/(1 + rhoList[[x]]))

  cvList <- lapply(meterV,
                   function(x) sqrt(sum(dt[meterID == x, sqError])/(nList[[x]] - p))/
                     mean(dt[meterID == x, elct], na.rm = TRUE))
  get_uFun <- function(r){
    U <- function(m, t, f){
      t*1.26*cvList[[r]]*sqrt((nList[[r]]/nRhoList[[r]])*(1 + 2/nList[[r]])/m)/f
    }
    U
  }
  uFunList <- lapply(meterV, function(x) get_uFun(x))
  return(uFunList)
}


