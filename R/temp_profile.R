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
ebPlot.tprofile = function(x)
{
  temp_range = seq(round(min(x$dat$temp)), round(max(x$dat$temp)), by = 0.1)
  dat = data.table(
    temp = temp_range,
    cd = as.numeric(temp_range > x$bc) * (temp_range - x$bc),
    hd = as.numeric(temp_range < x$bh) * (x$bh - temp_range))
  dat[, use:= predict(x$model, dat, type = 'response')]
  ggplot(data = dat, aes(x = temp, y = use)) +
    theme_light(base_size = 14) +
    geom_point(data = x$dat, aes(x = temp, y = use)) +
    geom_line(size = 1.5,
              color = c('Cooling' = 'royalblue',
                        'Heating' = 'red',
                        'Heating and Cooling' = 'green',
                        'Baseload Only' = 'black')[x$mtype])
}
