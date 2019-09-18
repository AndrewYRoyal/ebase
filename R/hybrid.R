#' Hybrid Method
#' @import data.table
#' @export
hybrid <- function(dat, ...) UseMethod('hybrid')

#' Hybrid Hourly
#' @import data.table
#' @export
hybrid.hourly <- function(dat, model_options,...)
{
  monthDict <- setNames(as.character(unique(dat$month)), as.character(unique(dat$month)))
  model_options$weights = 'obs_weights'
  out <- lapply(monthDict, function(month){
    m_upper <- as.character(as.numeric(month) + 1)
    m_lower <- as.character(as.numeric(month) - 1)
    if(!(m_upper %in% monthDict)) m_upper <- as.character(min(as.numeric(monthDict)))
    if(!(m_lower %in% monthDict)) m_lower <- as.character(max(as.numeric(monthDict)))
    weightDict <- setNames(c(0.5, 1, 0.5), c(m_lower, month, m_upper))
    mdat <- copy(dat)
    mdat[, obs_weights:= weightDict[as.character(month)]]
    mdat[is.na(obs_weights), obs_weights:= 0]
    gboost(mdat, model_options = model_options)
  })
  setattr(out, "class", c('hybrid', 'hourly'))
  return(out)
}

#' Hybrid Predict
#' @import data.table
#' @export
predict.hybrid <- function(mod, dat, ...)
{
  monthDict <- setNames(as.character(unique(dat$month)), as.character(unique(dat$month)))
  rbindlist(
    lapply(monthDict, function(m){
      predict(mod[[m]], dat[month == as.numeric(m), ])
    })
  )
}