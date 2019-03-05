#' Format Data
#' @export
formatData <- function(
  useDT, meterDT, modeledMeters, base.length, date.format, padding){
  useDT <- copy(useDT)
  meterDT <- copy(meterDT)

  if(!('nraStart' %in% names(meterDT))) meterDT[, nraStart:= as.POSIXct(NA)]
  if(!('nraEnd' %in% names(meterDT))) meterDT[, nraEnd:= as.POSIXct(NA)]
  if(is.character(useDT[, date])) useDT[, date:= as.POSIXct(date, format = date.format, tz = 'UTC')]
  if(is.character(meterDT[, inDate])) meterDT[, inDate:= as.POSIXct(inDate, format = date.format, tz = 'UTC')]
  if(is.character(meterDT[, nraStart])) meterDT[, nraStart:= as.POSIXct(nraStart, format = date.format, tz = 'UTC')]
  if(is.character(meterDT[, nraEnd])) meterDT[, nraEnd:= as.POSIXct(nraEnd, format = date.format, tz = 'UTC')]

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
    c('inStart', 'inEnd', 'blStart', 'pEnd', 'nraStart', 'nraEnd'))
  meterDT <- meterDT[, c('meterID', datVarV), with = FALSE]
  useDT <- merge(
    useDT,
    meterDT,
    by = 'meterID')

  maxDate <- max(useDT[, date])
  useDT[is.na(nraEnd) & !is.na(nraStart), nraEnd:= maxDate]
  useDT <- useDT[date >= blStart & date <= pEnd, ]
  useDT[, period:=
          as.numeric(date >= blStart) +
          as.numeric(date >= inStart) +
          as.numeric(date >= inEnd)]
  useDT[, period:= c('baseline', 'install', 'performance')[period]]
  useDT[, tbin:= pmin(ceiling(pmax(0, (temp - 50)/5)) + 1, 11)]
  for(t in unique(useDT[, tbin])) useDT[, paste0('tbin', t):= as.numeric(tbin == t)]
  useDT[, c('mm', 'dow'):= list(month(date), wday(date))]
  for(m in unique(useDT[, mm])) useDT[, paste0('mm', m):= as.numeric(mm == m)]
  useDT[, dHour:= .GRP, by = c('meterID', 'dow', 'hr')]
  useDT[, paste0('', datVarV):= NULL]
  return(useDT)
}

#' Panel Regression
#' @export
panelReg <- function(dt, meterV){
  tbinV <- c('<50', seq(55, 70, 5), '', seq(80, 95, 5), '>95')
  modelList <- lapply(meterV, function(x) panelModel(x, dt = dt))
  # modelFitV <- vapply(meterV,
  #                        function(x) panelList[[x]]$r2, FUN.VALUE = numeric(1))
  coefTableList <- lapply(modelList, function(x) coefTable(x))
  coefTableList <- lapply(
    names(coefTableList),
    function(x){
      coefTableList[[x]][, meterID:= x]
      setcolorder(coefTableList[[x]], c('meterID', 'coef', 'variable', 'value', 'stde', 'tvalue'))
    })
  coefTableDT <- rbindlist(coefTableList)
  return(list(modelList = modelList, coefTable = coefTableDT))
}

#' Panel Model
#' @export
panelModel <- function(m, dt){
  regDT <- dt[meterID == m, ]
  tbinV <- unique(regDT[, tbin])[-5] # set intercept on 5th bin
  nraI <- 'nra' %in% names(regDT) && !anyNA(regDT[, nra])

  rForumula <- paste0('log(elct + .01)', '~',
                      paste0('tbin', tbinV, collapse = '+'),
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
rPredict <- function(meter, dt, nra.est){
  regDT <- dt[meterID == meter, ]
  mmV <- unique(regDT[, mm])[-1]
  tbinV <- unique(regDT[, tbin])[-5]

  rFormula <- paste0('elctDM ~',
                     paste0('mm', mmV, 'DM', collapse = '+'), '+',
                     paste0('tbin', tbinV, 'DM', collapse = '+'),
                     ifelse(nra.est && !anyNA(dt[, nra]), '+ nra', ''))
  varV <- as.list(c('elct', paste0('mm', mmV), paste0('tbin', tbinV)))
  names(varV) <- varV

  regDT[period == 'baseline',
        paste0(varV, 'm'):=
          lapply(varV,
                 function(x) mean(get(x), na.rm = TRUE)),
        by = .(dHour)]
  regDT <- merge(
    regDT,
    unique(regDT[period == 'baseline',
                 c('dHour', paste0(varV, 'm')),
                 with = FALSE]),
    by = 'dHour', suffixes = c('', '.pred'))
  regDT[,
        paste0(varV, 'DM'):=
          lapply(varV,
                 function(x) get(x) - get(paste0(x, 'm.pred')))]
        # by = c('dHour', 'baseline', 'meterID')]

  est <- lm(rFormula, data = regDT[period == 'baseline', ])
  regDT[, pElct:= elctm.pred + predict(est, regDT)]
  return(regDT[, .(meterID, date, hr, pElct)])
}

#' Gradient Boost Predictions
#' @export
mlPredict <- function(meter, dt, nra.est){
  regDT <- dt[meterID == meter, ]
  regDT <- regDT[, dHour:= as.factor(dHour)]
  mmV <- unique(regDT[, mm])[-1]

  keepV <- c('meterID', 'elct', 'period', 'date', 'hr', 'temp', 'dHour', paste0('mm', mmV))
  if(nra.est && !anyNA(regDT[, nra])) keepV <- c(keepV, 'nra')
  regDT <- regDT[, keepV, with = FALSE]

  baselineM <- xgb.DMatrix(
    data = sparse.model.matrix(elct ~ ., data = regDT[period == 'baseline',
                                                      -c('meterID', 'period', 'date', 'hr')]),
    label = regDT[period == 'baseline', elct]
  )

  cb.cv.predict(save_models = TRUE)
  nIterate <- xgb.cv(data = baselineM,
                     nfold = 5, max_depth = 5, eta = .1, nthread = 8, nrounds = 2000,
                     early_stopping_rounds = 3, objective = 'reg:linear', verbose = 0)$best_iteration
  gbModel <- xgboost(data = baselineM,
                     max_depth = 5, eta = .1, nthread = 8, nrounds = nIterate,
                     early_stopping_rounds = 3, objective = 'reg:linear', verbose = 0)
  regDT[period == 'baseline', MLpElct:= predict(gbModel, baselineM)]

  if(uniqueN(regDT[period != 'baseline', ]) > 30){
    perfM <- xgb.DMatrix(
      data = sparse.model.matrix(elct ~ ., data = regDT[period != 'baseline',
                                                        -c('meterID', 'period', 'date', 'hr', 'MLpElct')]),
      label = regDT[period != 'baseline', elct]
    )
    regDT[period != 'baseline', MLpElct:= predict(gbModel, perfM)]
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


