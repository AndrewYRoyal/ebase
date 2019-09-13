#' Regression Method
#' @import data.table
#' @export
regress <- function(dat, ...) UseMethod('regress')

#' Hourly Regression
#' @import data.table
#' @export
regress.hourly <- function(dat, model_options, ...){
  dat <- copy(dat)
  if(is.null(model_options$weights)) dat[, obs_weights:= 1] else setnames(dat, model_options$weights, 'obs_weights')
  reg_formula <- quote(use ~ tow + tbin + mm - 1)
  model_type = 'TOWTM'
  if(uniqueN(dat$tbin) < 2){ reg_formula <- quote(use ~ tow + mm - 1); model_type = 'TOWM'}
  if(uniqueN(dat$mm) < 2){ reg_formula <- quote(use ~ tow + tbin - 1); model_type = 'TOWT'}
  if(uniqueN(dat$mm) < 2 & uniqueN(dat$tbin) < 2){ reg_formula <- quote(use ~ tow); model_type = 'TOW'}
  if(!is.null(model_options$custom_lm)){ reg_formula <- model_options$custom_lm; model_type = 'custom'}
  dat[, tow:= as.factor(tow)]
  mod <- lm(reg_formula,
            data = dat,
            weights = obs_weights)
  out <- list(mod = mod,
              towMeans = unique(dat[, .(tow, use)]), # = mUse)]),
              model_type = model_type,
              weights = !is.null(model_options$weights))
  structure(out, class = 'regress')
}

#' Regression Predict
#' @import data.table
#' @export
predict.regress <- function(mod, dat, ...){
  dat <- copy(dat)
  tryCatch(
  {
    mmLevels <- setNames(mod$mod$xlevels$mm, mod$mod$xlevels$mm)
    msngLevels <- setdiff(unique(dat$mm), mmLevels); names(msngLevels) <- msngLevels
    if(length(msngLevels) > 0){
      lnnDict <- sapply(msngLevels, function(x){
        as.character(mmLevels[which.min(abs(as.numeric(mmLevels) - as.numeric(x)))])
      })
      dat[mm %in% msngLevels, mm:= lnnDict[as.character(mm)]]
    }
  }, error = function(e) NA)

  dat[, tow:= as.factor(tow)]
  dat[, pUse:= predict(mod$mod, dat)]
  dat[, .(meterID, date, period, use, pUse)]
}

