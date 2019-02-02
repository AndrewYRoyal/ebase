#' Format Data
#' @export
get_dataFormat <- function(dt, id.var, use.var, date.var, hour.var, temp.var, install.date.var,
                           nra.start.var, nra.end.var,
                           base.length, date.format){
  dt <- copy(dt)
  nOldV <- match.call()
  nSetV <- c('id.var', 'use.var', 'date.var', 'hour.var', 'temp.var', 'install.date.var',
                'nra.start.var', 'nra.end.var')
  nNewV <- c(id.var = 'meterID', use.var = 'elct', date.var = 'date', hour.var = 'hr',
                  temp.var = 'temp', install.date.var = 'inDate', nra.start.var = 'nraStart',
                  nra.end.var = 'nraEnd')
  nOldV <- nOldV[names(nOldV) %in% nSetV]
  nNewV <- nNewV[names(nOldV)]
  nOldV <- as.character(unlist(nOldV))
  setnames(dt, nOldV, nNewV)
  dt <- na.omit(dt, c('date', 'elct'))

  if(!('nraStart' %in% names(dt))) dt[, nraStart:= NA]
  if(!('nraEnd' %in% names(dt))) dt[, nraEnd:= NA]

  if(is.character(dt[, date])) dt[, date:= as.POSIXct(date, format = date.format, tz = 'UTC')]
  mDatesDT <- unique(dt[, .(meterID, inDate, nraStart, nraEnd)])
  if(is.character(mDatesDT[, inDate])) mDatesDT[, inDate:= as.POSIXct(inDate, format = date.format, tz = 'UTC')]
  if(is.character(mDatesDT[, nraStart])) mDatesDT[, nraStart:= as.POSIXct(nraStart, format = date.format, tz = 'UTC')]
  if(is.character(mDatesDT[, nraEnd])) mDatesDT[, nraEnd:= as.POSIXct(nraEnd, format = date.format, tz = 'UTC')]

  for(r in unique(mDatesDT[, meterID])){
    mDatesDT[meterID == r, blStartDate:= seq(inDate,
                                             length = 2,
                                             by = paste0("-", base.length, " months"))[2]]
    mDatesDT[meterID == r, yoyDate:= seq(inDate,
                                         length = 2,
                                         by = paste0("-", 1, " months"))[2]]
  }

  dt <- merge(dt[, .(meterID, elct, date, hr, temp)], mDatesDT)

  dt[, c('yr', 'yday', 'mm', 'mday', 'dow'):= list(as.POSIXlt(date)$year + 1900,
                                                   as.POSIXlt(date)$yday,
                                                   as.POSIXlt(date)$mon + 1,
                                                   as.POSIXlt(date)$mday,
                                                   as.POSIXlt(date)$wday)]
  maxDate <- max(dt[, date])
  dt[, mm:= as.factor(mm)]
  dt[, baseline:= ifelse(date >= blStartDate & date < inDate, 1, 0)]
  dt[, postECM:= ifelse(date >= inDate, 1, 0)]
  dt[is.na(nraEnd) & !is.na(nraStart), nraEnd:= maxDate]
  dt[, nra:= ifelse(date >= nraStart & date <= nraEnd, 1, 0)]
  dt <- dt[baseline == 1 | postECM == 1, ]
  dt[, yoy:= ifelse(date >= yoyDate, 1, 0)]
  dt[, c('inDate', 'blStartDate', 'yoyDate', 'nraStart', 'nraEnd'):= NULL]
  dt[, paste0('tbin', 1:10):= list(ifelse(temp <= 50, 1, 0),
                                   ifelse(inrange(temp, 51, 55), 1, 0),
                                   ifelse(inrange(temp, 56, 60), 1, 0),
                                   ifelse(inrange(temp, 61, 65), 1, 0),
                                   ifelse(inrange(temp, 66, 70), 1, 0),
                                   #ifelse(inrange(temp, 71, 75), 1, 0),
                                   ifelse(inrange(temp, 76, 80), 1, 0),
                                   ifelse(inrange(temp, 81, 85), 1, 0),
                                   ifelse(inrange(temp, 86, 90), 1, 0),
                                   ifelse(inrange(temp, 91, 95), 1, 0),
                                   ifelse(temp >= 96, 1, 0))]
  dt[, dHour:= .GRP, by = c('meterID', 'dow', 'hr')]
  return(dt)
}

#' Uncertainty Function List
#' @export
get_uFunList <- function(dt){
  meterV <- unique(dt[, meterID]); names(meterV) <- meterV
  p <- 190 # 7x24 + 12 + 10
  dt <- copy(dt[baseline == 1, ])[order(meterID, yr, yday, hr)]
  dt[, lagElct:= shift(elct, 1, type = 'lag'), by = 'meterID']
  dt[, sqError:= (elct - pElct)^2]

  rhoList <- lapply(meterV, function(x) cor(dt[!is.na(lagElct) & meterID == x, elct],
                                            dt[!is.na(lagElct) & meterID == x, lagElct]))

  nList <- lapply(meterV, function(x) uniqueN(dt[meterID == x, ]))
  nPList <- lapply(meterV, function(x) nList[[x]]*(1 - rhoList[[x]])/(1 + rhoList[[x]]))

  cvList <- lapply(meterV,
                   function(x) sqrt(sum(dt[meterID == x, sqError/(nList[[x]] - p)]))/
                     mean(dt[meterID == x, elct], na.rm = TRUE))
  get_uFun <- function(r){
    U <- function(m, t, f){
      t*1.26*cvList[[r]]*sqrt((nList[[r]]/nPList[[r]])*(1 + 2/nList[[r]])/m)/f
    }
    U
  }
  uFunList <- lapply(meterV, function(x) get_uFun(x))
  return(uFunList)
}

#' Panel Regression
#' @export
get_panelReg <- function(m, dt){
  regDT <- copy(dt[meterID == m, ])
  yoyI <- uniqueN(regDT[baseline == 1, .(mm, yr)]) >= 13
  nraI <- 'nra' %in% names(regDT) && !anyNA(regDT[, nra])
  rForumula <- paste0('log(elct + .01)', '~', paste0('tbin', 1:10, collapse = '+'),
                      ifelse(yoyI, '+ yoy', ''),
                      ifelse(nraI, ' + nra', ''),
                      '- 1 | dHour + mm | 0 | 0')
  out = do.call('felm', list(as.formula(rForumula), data = as.name('regDT')))
  return(out)
}

#' Coefficient Summary
#' @export
get_coefSummary <- function(x){
  estM <- coef(summary(x))
  estM <- cbind(coef = rownames(estM), estM)
  estDT <- as.data.table(estM[, c('coef', 'Estimate', 'Std. Error', 't value')])
  numV <- setdiff(names(estDT), c('coef', 'meterID'))
  estDT[, paste0('', numV):= lapply(numV, function(x) as.numeric(get(x)))]
  setnames(estDT, names(estDT), c('coef', 'est', 'stde', 'tvalue'))
  estDT[, est:= sapply(est, function(x) get_pctTransform(x))]
  estDT[, stde:= mapply(get_pctTransform, est, stde)]
  estDT[, c('ub', 'lb'):= list(est + 1.96*stde, est - 1.96*stde)]
  estDT <- melt(estDT, id.vars = c('coef', 'tvalue', 'stde'))
  estDT[, meterID:= names(x)[1]]
  setcolorder(estDT, c('meterID', 'coef', 'variable', 'value', 'stde', 'tvalue'))
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
get_rPredict <- function(dt, base.length, yoy.adjust, nra.est){
  meterV <- unique(dt[, meterID]); names(meterV) <- meterV
  varV <- as.list(c('elct', paste0('tbin', 1:10)), 'yoy')
  names(varV) <- varV

  dt[, paste0('M', varV):= lapply(varV, function(x) mean(get(x), na.rm = TRUE)),
     by = c('dHour', 'baseline', 'meterID')]
  dt[, paste0('DM', varV):= lapply(varV, function(x) get(x) - mean(get(x), na.rm = TRUE)),
     by = c('dHour', 'baseline', 'meterID')]

  get_rFormula <- function(x){
    paste0('DMelct', '~',
           paste0('DMtbin', 1:10, collapse = '+'),
           '+ as.factor(mm)', ifelse(base.length > 11 & yoy.adjust, '+ yoy', ''),
           ifelse(nra.est && !anyNA(dt[meterID == x, nra]), '+ nra', ''), '- 1')
  }
  rFormulaV <- sapply(meterV, function(x) get_rFormula(x))

  estList <- lapply(meterV, function(x) lm(rFormulaV[x], data = dt[meterID == x & baseline == 1, ]))
  dt <- merge(dt, unique(dt[baseline == 1, .(meterID, mm, dHour, Melct)]),
              by = c('meterID', 'mm', 'dHour'), suffixes = c('', '.pred'))
  for(m in names(estList)) dt[meterID == m,
                              pElct:= Melct.pred + predict(estList[[m]], dt[meterID == m, ])]
  return(dt[, .(meterID, date, hr, pElct)])
}


#' Gradient Boost Predictions
#' @export
get_mlPredict <- function(dt, r, nra.est){
  dt <- dt[meterID == r, ]
  keepV <- c('meterID', 'baseline', 'date', 'yr', 'yday', 'mm', 'hr', 'dHour', 'elct', 'temp')
  if(nra.est && !anyNA(dt[, nra])) keepV <- c(keepV, 'nra')
  dt <- dt[, keepV, with = FALSE]

  baselineM <- xgb.DMatrix(
    data = sparse.model.matrix(elct ~ ., data = dt[baseline == 1, -c('date', 'yday', 'hr', 'baseline', 'meterID')])[, -1],
    label = dt[baseline == 1, elct]
  )

  cb.cv.predict(save_models = TRUE)
  nIterate <- xgb.cv(data = baselineM, nfold = 10,
                     max_depth = 6, eta = .5, nthread = 8, nrounds = 2000,
                     early_stopping_rounds = 5, objective = 'reg:linear', verbose = 0)$best_iteration
  gbModel <- xgboost(data = baselineM,
                     max_depth = 6, eta = .5, nthread = 8, nrounds = nIterate,
                     early_stopping_rounds = 5, objective = 'reg:linear', verbose = 0)
  dt[baseline == 1, MLpElct:= predict(gbModel, baselineM)]

  if(uniqueN(dt[baseline == 0, ]) > 30){
    postM <- xgb.DMatrix(
      data = sparse.model.matrix(elct ~ ., data = dt[baseline == 0, -c('date', 'yday', 'hr', 'baseline', 'meterID', 'MLpElct')])[, -1],
      label = dt[baseline == 0, elct]
    )
    dt[baseline == 0, MLpElct:= predict(gbModel, postM)]
  }

  return(dt[, .(meterID, date, hr, MLpElct)])
}

