#' CalTrack Method
#' @import data.table
#' @export
caltrack <- function(dat, ...) UseMethod('caltrack')

#' CalTrack Hourly
#' @import data.table
#' @export
caltrack.hourly <- function(dat, model_options, ...){
  mmDict <- setNames(as.character(unique(dat$mm)), as.character(unique(dat$mm)))
  custom_lm = 'use ~ tbin - 1'
  if(model_options$occupancy_lookup) custom_lm = 'use ~ tbin:occupied - 1'
  out <- lapply(mmDict, function(month){
    m_upper <- as.character(as.numeric(month) + 1)
    m_lower <- as.character(as.numeric(month) - 1)
    if(!(m_upper %in% mmDict)) m_upper <- as.character(min(as.numeric(mmDict)))
    if(!(m_lower %in% mmDict)) m_lower <- as.character(max(as.numeric(mmDict)))
    weightDict <- setNames(c(0.5, 1, 0.5), c(m_lower, month, m_upper))
    mdat <- copy(dat)
    mdat[, obs_weights:= weightDict[as.character(mm)]]
    mdat[is.na(obs_weights), obs_weights:= 0]
    regress(mdat, model_options = list(weights = 'obs_weights', custom_lm = custom_lm))
  })
  setattr(out, "class", c('caltrack', 'hourly'))
  return(out)
}

#' CalTrack Predict
#' @import data.table
#' @export
predict.caltrack <- function(mod, dat, ...){
  mmDict <- setNames(as.character(unique(dat$mm)), as.character(unique(dat$mm)))

  rbindlist(
    lapply(mmDict, function(m){
      predict(mod[[m]], dat[mm == m, ])
    })
  )
}

