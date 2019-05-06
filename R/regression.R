#' Regression Method
#' @import data.table
#' @export
regress <- function(x) UseMethod('regress')

#' Hourly Regression
#' @import data.table
#' @export
regress.hourly <- function(x){
  varV <- c('use', paste0('tbin', 1:11), paste0('mm', 1:12))
  dat <- copy(x$dat[['baseline']])
  dat <- regFormat(dat)
  mod <- lm(use ~ . - tow,
            data = dat[, lapply(.SD,
                                function(x) x - mean(x)),
                       .SDcols = varV,
                       by = .(tow)])
  dat <- dat[, lapply(.SD, mean),
             .SDcols = varV,
             by = .(tow)]
  out <- list(mod = mod, meanDT = dat, varV = varV)
  structure(out, class = 'regress')
}
