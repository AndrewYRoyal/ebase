#' Regression Method
#' @import data.table
#' @export
regress <- function(dat, ...) UseMethod('regress')

#' Hourly Regression
#' @import data.table
#' @export
regress.hourly <- function(dat, ...){
  dat <- copy(as.data.table(dat))
  mod <- lm(use ~ tbin + mm, data = dat[, .(use = use - mean(use),
                                            tbin = tbin,
                                            mm = mm),
                                        by = .(tow)])
  out <- list(mod = mod, towMeans = dat[, .(use = mean(use)), by = .(tow)])
  structure(out, class = 'regress')
}

#' Regression Predict
#' @import data.table
#' @export
predict.regress <- function(mod, dat, ...){
  dat <- copy(as.data.table(dat))

  mmLevels <- setNames(mod$mod$xlevels$mm, mod$mod$xlevels$mm)
  msngLevels <- setdiff(levels(dat$mm), mmLevels); names(msngLevels) <- msngLevels

  if(length(msngLevels) > 0){
    lnnDict <- sapply(msngLevels, function(x){
      as.character(mmLevels[which.min(abs(as.numeric(mmLevels) - as.numeric(x)))])
    })
    dat[mm %in% msngLevels, mm:= lnnDict[as.character(mm)]]
  }

  predDT <- merge(
    dat,
    mod$towMeans,
    by = 'tow',
    suffixes = c('', '.mean'))
  predDT[, pUse:= use.mean + predict(mod$mod, predDT[, .(use = use - use.mean, tbin, mm)])]
  predDT[, .(meterID, date, period, use, pUse)]
}

