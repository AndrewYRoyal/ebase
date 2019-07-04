#' Regression Method
#' @import data.table
#' @export
regress <- function(dat, ...) UseMethod('regress')

#' Hourly Regression
#' @import data.table
#' @export
regress.hourly <- function(dat, pSet, ...){
  dat <- copy(as.data.table(dat))
  if(is.null(pSet$weights)) dat[, obs_weights:= 1] else setnames(dat, pSet$weights, 'obs_weights')
  reg_formula <- quote(use ~ tbin + mm)
  if(uniqueN(dat$tbin) < 2) reg_formula <- quote(use ~ mm)
  if(uniqueN(dat$mm) < 2) reg_formula <- quote(use ~ tbin)
  if(uniqueN(dat$mm) < 2 & uniqueN(dat$tbin) < 2) reg_formula <- quote(use ~ 1)
  mod <- lm(reg_formula,
            data = dat[, .(use = use - weighted.mean(use, obs_weights),
                           tbin = tbin,
                           mm = mm,
                           obs_weights = obs_weights),
                       by = .(tow)],
            weights = obs_weights)
  out <- list(mod = mod, towMeans = dat[, .(use = mean(use)), by = .(tow)])
  structure(out, class = 'regress')
}

#' Regression Predict
#' @import data.table
#' @export
predict.regress <- function(mod, dat, ...){
  dat <- copy(as.data.table(dat))

  tryCatch({
    mmLevels <- setNames(mod$mod$xlevels$mm, mod$mod$xlevels$mm)
    msngLevels <- setdiff(levels(dat$mm), mmLevels); names(msngLevels) <- msngLevels
    if(length(msngLevels) > 0){
      lnnDict <- sapply(msngLevels, function(x){
        as.character(mmLevels[which.min(abs(as.numeric(mmLevels) - as.numeric(x)))])
      })
      dat[mm %in% msngLevels, mm:= lnnDict[as.character(mm)]]
    }
  }, error = function(e) cat(unique(dat$meterID), ': No Monthly Effects \n'))

  predDT <- merge(
    dat,
    mod$towMeans,
    by = 'tow',
    suffixes = c('', '.mean'))
  predDT[, pUse:= use.mean + predict(mod$mod, predDT[, .(use = use - use.mean, tbin, mm)])]
  predDT[, .(meterID, date, period, use, pUse)]
}

