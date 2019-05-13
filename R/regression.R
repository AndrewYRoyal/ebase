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
  predDT <- merge(
    dat,
    mod$towMeans,
    by = 'tow',
    suffixes = c('', '.mean'))
  predDT[, pUse:= use.mean + predict(mod$mod, predDT[, .(use = use - use.mean, tbin, mm)])]
  predDT[, .(meterID, date, period, use, pUse)]
}

