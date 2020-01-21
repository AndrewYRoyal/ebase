#' Retrieve Temperature Profile
#' @import data.table
#' @export
ebTempProf = function(dat, balance_points = NULL, dm_vars = NULL)
{
  dat = copy(dat)
  if(!is.null(dm_vars)) dat = dat[, .(use = use - mean(use), temp = temp), by = dm_vars]
  if(is.null(balance_points))
  {
    balance_points = as.data.table(expand.grid(bh = seq(40, 70, 3),
                                               bc = seq(60, 90, 3)))
    balance_points = balance_points[bh < bc]
    balance_points = rbindlist(
      list(balance_points[, .(bh = bh, bc = bc, frmla = 'use ~ cd + hd')],
           unique(balance_points[, .(bh = NA, bc = bc, frmla = 'use ~ cd')]),
           unique(balance_points[, .(bh = bh, bc = NA, frmla = 'use ~ hd')])))
  }
  balance_points[, r2:= mapply(
    function(bh, bc, frmla)  {
      lmodel = lm(frmla,
                  data = dat[, .(use = use,
                                 temp,
                                 cd = as.numeric(temp > bc) * (temp - bc),
                                 hd = as.numeric(temp < bh) * (bh - temp))])
      if(anyNA(lmodel$coefficients)) return(0)
      tvalues = summary(lmodel)$coefficients[, c('t value')]
      if(min(tvalues[c('cd', 'hd')], na.rm = TRUE) < 0) return(0)
      summary(lmodel)$r.squared
    },
    bh = bh, bc = bc, frmla = frmla)]
  bc_opt = balance_points[which.max(r2)]$bc
  bh_opt = balance_points[which.max(r2)]$bh
  frmla = balance_points[which.max(r2)]$frmla
  r2 = max(balance_points$r2)
  if(r2 < 0.10) frmla = 'use ~ 1'
  dat = dat[, .(use = use,
                temp,
                cd = as.numeric(temp > bc_opt) * (temp - bc_opt),
                hd = as.numeric(temp < bh_opt) * (bh_opt - temp))]
  lmodel = lm(frmla, data = dat)
  mtype = c('use ~ cd + hd' = 'Heating and Cooling',
            'use ~ cd' = 'Cooling',
            'use ~ hd' = 'Heating',
            'use ~ 1' = 'Baseload Only')[frmla]

  out = list(model = strip_lm(lmodel),
             bc = bc_opt,
             bh = bh_opt,
             mtype = mtype,
             r2 = r2,
             dat = dat[, .(use, temp)])
  setattr(out, "class", c('tprofile', class(out)))
  out
}


#' Plot Temperature Profile
#' @import data.table
#' @import ggplot2
#' @export
ebPlot.tprofile = function(x, data_only = FALSE)
{
  temp_range = seq(round(min(x$dat$temp)), round(max(x$dat$temp)), by = 0.1)
  dat = data.table(
    temp = temp_range,
    cd = as.numeric(temp_range > x$bc) * (temp_range - x$bc),
    hd = as.numeric(temp_range < x$bh) * (x$bh - temp_range))
  dat[, use:= predict(x$model, dat, type = 'response')]
  if(data_only) return(list(curve = dat[, .(temp, use, mtype = x$mtype)], points = x$dat))
  ggplot(data = dat, aes(x = temp, y = use)) +
    theme_light(base_size = 14) +
    geom_point(data = x$dat, aes(x = temp, y = use)) +
    scale_x_continuous('Outside Temperature') +
    scale_y_continuous('kWh') +
    geom_line(size = 1.5,
              color = c('Cooling' = 'royalblue',
                        'Heating' = 'red',
                        'Heating and Cooling' = 'darkorange',
                        'Baseload Only' = 'black')[x$mtype])
}

#' Facet Plot Hourly Temperature Profile
#' @import data.table
#' @import ggplot2
#' @export
ebHourlyTempFacet <- function(dat, hours = c(0:11 * 2))
{
  hour_levels = c(paste0(c(12, 1:11), 'AM'), paste0(c(12, 1:11), 'PM'))
  balance_points = as.data.table(expand.grid(bh = seq(40, 70, 3),
                                             bc = seq(60, 90, 3)))
  balance_points = balance_points[bh < bc]
  balance_points = rbindlist(
    list(balance_points[, .(bh = bh, bc = bc, frmla = 'use ~ cd + hd')],
         unique(balance_points[, .(bh = NA, bc = bc, frmla = 'use ~ cd')]),
         unique(balance_points[, .(bh = bh, bc = NA, frmla = 'use ~ hd')])))

  tempList = lapply(hours, function(hr){
    out = ebPlot(ebTempProf(dat = dat[hour(date) == hr, ], balance_points = balance_points),
                 data_only = TRUE)
    lapply(out, function(x) x[, hour:= hr])
  })
  curve_dat = rbindlist(lapply(tempList, function(x) x[['curve']]))
  points_dat = rbindlist(lapply(tempList, function(x) x[['points']]))

  curve_dat[, hour:= factor(hour_levels[hour + 1], levels = hour_levels)]
  points_dat[, hour:= factor(hour_levels[hour + 1], levels = hour_levels)]

  colorDict = c('Cooling' = 'royalblue',
                'Heating' = 'red',
                'Heating and Cooling' = 'darkorange',
                'Baseload Only' = 'black')
  ggplot(data = curve_dat, aes(x = temp, y = use, color = mtype)) +
    theme_light(base_size = 15) +
    theme(legend.position = 'top') +
    geom_line(size = 0.5) +
    geom_point(data = points_dat, color = 'gray', aes(x = temp, y = use)) +
    geom_line(size = 1.5) +
    scale_color_manual('', values = colorDict) +
    scale_x_continuous('Outside Temperature') +
    scale_y_continuous('kWh') +
    facet_wrap(~hour)
}

#' Predict site-year heating and cooling load
#' @import data.table
#' @export
predict.tprofile = function(x, dat) {
  min_use = min(x$dat$use)
  dat = dat[, .(site, date, hd = pmax(x$bh - temp, 0), cd = pmax(temp - x$bc, 0))]
  dat[, p_use:= predict(x$model, dat)]

  cooling = dat[cd > 0, .(cooling = sum(p_use - min_use)), by = .(site, year(date))]
  heating = dat[hd > 0, .(heating = sum(p_use - min_use)), by = .(site, year(date))]

  merge(cooling, heating, by = c('site', 'year'))
}

