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
  }
  balance_points[, r2:= mapply(
    function(bh, bc)
    {
      lmodel = lm('use ~ cd + hd',
                  data = dat[, .(use = use,
                                 temp,
                                 cd = as.numeric(temp > bc) * (temp - bc),
                                 hd = as.numeric(temp < bh) * (bh - temp))])
      if(min(summary(lmodel)$coefficients[, 't value'], na.rm = TRUE) < -1.96) return(0)
      summary(lmodel)$r.squared
    },
    bh = bh, bc = bc)]
  
  bc_opt = balance_points[which.max(r2)]$bc
  bh_opt = balance_points[which.max(r2)]$bh
  
  dat = dat[, .(use = use,
                temp,
                cd = as.numeric(temp > bc_opt) * (temp - bc_opt),
                hd = as.numeric(temp < bh_opt) * (bh_opt - temp))]
  lmodel = lm('use ~ cd + hd', data = dat)
  t_values = abs(summary(lmodel)$coefficients[, 't value'])
  t_values = c(t_values, c('cd' = 0, 'hd' = 0)[setdiff(c('cd', 'hd'), names(t_values))])
  sig_coef = c('Cooling' = t_values[['cd']] > 1.96,
               'Heating' = t_values[['hd']] > 1.96)
  if(sum(sig_coef) == 1){
    mtype = names(sig_coef)[sig_coef]
    lmodel = lm(sprintf('use ~ %s', c('Cooling' = 'cd', 'Heating' = 'hd')[mtype]), data = dat)
  } else if(sum(sig_coef) == 2){
    mtype = 'Heating and Cooling'
  } else if(sum(sig_coef) == 0 || max(balance_points$r2)){
    mtype = 'Baseload Only'
    lmodel = lm('use ~ 1', data = dat)
  }
  out = list(model = lmodel,
             bc = bc_opt,
             bh = bh_opt,
             mtype = mtype,
             r2 = max(balance_points$r2),
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
