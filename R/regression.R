#' Regression Method
#' @import data.table
#' @export
regress <- function(dat, ...) UseMethod('regress')

#' Hourly Regression
#' @import data.table
#' @export
regress.hourly <- function(dat, model_options, ...) {
  dat <- copy(dat)
  if(is.null(model_options$weights)) dat[, obs_weights:= 1] else setnames(dat, model_options$weights, 'obs_weights')
  reg_formula <- quote(use ~ tow + tbin + mm - 1)
  model_type = 'TOWTM'
  if(uniqueN(dat$tbin) < 2){ reg_formula <- quote(use ~ tow + mm - 1); model_type = 'TOWM'}
  if(uniqueN(dat$mm) < 2){ reg_formula <- quote(use ~ tow + tbin - 1); model_type = 'TOWT'}
  if(uniqueN(dat$mm) < 2 & uniqueN(dat$tbin) < 2){ reg_formula <- quote(use ~ tow); model_type = 'TOW'}
  if(!is.null(model_options$custom_lm)){ reg_formula <- model_options$custom_lm; model_type = 'custom'}
  dat[, tow:= as.factor(tow)]
  mod <- strip_lm(lm(reg_formula,
                     data = dat,
                     weights = obs_weights,
                     model = FALSE,
                     y = FALSE))
  out <- list(mod = mod,
              model_type = model_type,
              weights = !is.null(model_options$weights))
  structure(out, class = 'regress')
}

#' Regression Predict
#' @import data.table
#' @export
predict.regress <- function(mod, dat, ...)
{
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
  setattr(dat, name = 'class', value = c('hourly_predict', class(dat)))
  dat[, .(meterID, date, period, use, pUse)]
}


#' Forecast Method
#' @import data.table
#' @export
ebForecast.regress <- function(model, dat, ...) {
  mod <- model$mod
  dat <- copy(dat)
  interval = class(dat)[1]
  towDict <- get_towDict(interval)
  if(interval == 'hourly') {
    dat <- dat[, .(date = date,
                   temp = temp,
                   tow = factor(towDict[paste0(weekdays(date), hour(date))], levels = mod$xlevels$tow),
                   mm = as.factor(month(date)))]
  } else if(interval == 'daily'){
    dat <- dat[, .(date = date,
                   temp = temp,
                   tow = factor(towDict[weekdays(date)], levels = mod$xlevels$tow),
                   mm = as.factor(month(date)))]
  }
  tryCatch(
    {
      mmLevels <- setNames(mod$xlevels$mm, mod$xlevels$mm)
      msngLevels <- setdiff(unique(dat$mm), mmLevels); names(msngLevels) <- msngLevels
      if(length(msngLevels) > 0){
        lnnDict <- sapply(msngLevels, function(x){
          as.character(mmLevels[which.min(abs(as.numeric(mmLevels) - as.numeric(x)))])
        })
        dat[mm %in% msngLevels, mm:= lnnDict[as.character(mm)]]
      }
    }, error = function(e) NA) # TODO: also appears in ebPredict-- make more modular
  get_levels <- Vectorize(FUN = function(x) regmatches(x, regexpr('(?<=,).+(?=])', x, perl = TRUE)))
  tcuts <- c(-Inf, as.numeric(get_levels(mod$xlevels$tbin)))
  dat[, tbin:= cut(temp, tcuts)]
  dat[, .(date, temp, use = predict(mod, dat))]
}
