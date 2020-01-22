#' Format Savings from Summary Object
#' @import data.table
#' @export
ebSavings = function(x, deemed, groups = NULL, sites_subset = NULL) {
  dat = merge(
    x$sites$savings[, .(site, Gross = gross, var_gross)],
    x$sites$metrics[, .(site, Baseline = baseline)])
  dat[, Deemed:= deemed[as.character(site)]]
  dat[, norm:= FALSE]
  if(!is.null(x$sites$norm)) {
    norm_dat = merge(
      x$sites$norms[, .(site, Gross = gross, var_gross)],
      x$sites$metrics[, .(site, Baseline = baseline)])
    norm_dat[, Deemed:= deemed[as.character(site)]]
    norm_dat[, norm:= TRUE]
    dat = rbind(dat, norm_dat)
  }
  if(!is.null(sites_subset)) dat = dat[site %in% sites_subset]
  cols = c('Gross', 'Baseline', 'Deemed', 'var_gross')
  dat = dat[, lapply(.SD, as.numeric), .SDcols = cols, by = .(id = site, norm)]
  if(!is.null(groups)) dat = dat[, lapply(.SD, sum, na.rm = TRUE), .SDcols = cols,
                                 by = .(id = groups[as.character(id)], norm)]

  formatted = dat[!(norm),
                  .(id = id,
                    Baseline = format(round(Baseline), big.mark = ','),
                    Deemed = format(round(Deemed), big.mark = ','),
                    `Deemed/Baseline` = round(Deemed / Baseline, 2),
                    Gross = format(round(Gross), big.mark = ','),
                    RRate = round(Gross / Deemed, 2))]
  out = list(formatted = formatted,
             raw = dat)
  setattr(out, 'class', c('savings', 'list'))
  out
}

#' Summarize ECM Savings
#' @import data.table
#' @export
ebSavings_ECM = function(x, site_dat, measures, reporting_subset = NULL) {
  dat = merge(
    x$raw,
    site_dat[, .SD, .SDcols = c('site', measures)],
    by.x = 'id', by.y = 'site')

  gross_frm = sprintf('Gross ~ %s  - 1', paste(measures, collapse = '+'))
  gross_model = summary(lm(gross_frm, data = dat))
  deemed_frm = sprintf('Deemed ~ %s  - 1', paste(measures, collapse = '+'))
  deemed_model = summary(lm(deemed_frm, data = dat))
  if(!is.null(reporting_subset)) measures = reporting_subset
  dat = data.table(id = measures,
                   norm = FALSE,
                   Gross = gross_model$coefficients[measures, 1],
                   Deemed = deemed_model$coefficients[measures, 1],
                   var_gross = gross_model$coefficients[measures, 2] ^2)
  formatted = dat[, .(id = id,
                      Deemed = format(round(Deemed), big.mark = ','),
                      Gross = format(round(Gross), big.mark = ','),
                      RRate = round(Gross / Deemed, 2))]
  out = list(formatted = formatted,
             raw = dat)
  setattr(out, 'class', c('savings', 'list'))
  out
}

#' Plot Savings
#' @import data.table
#' @export
ebPlot.savings = function(x, units = 'kWh', label = FALSE) {
  dat = melt(x$raw[, -c('var_gross')], id.vars = c('id', 'norm'), value.name = 'use')
  dat[variable %in% c('Baseline', 'Deemed'), norm:= FALSE]
  dat = unique(na.omit(dat))
  dat = merge(dat,
              x$raw[, .(variable = 'Gross', id, var_gross, norm, Deemed)],
              by = c('id', 'variable', 'norm'),
              all.x = TRUE)
  dat[(norm), variable:= 'Norm']
  dat[, rr:= round(use / Deemed, 2)]
  dat[, use:= use / 1e3]
  dat[, var_gross:= var_gross / 1e6]
  dat[is.na(var_gross), var_gross:= 0]
  dat[, variable:= factor(variable, levels = c('Baseline', 'Deemed', 'Gross', 'Norm'))]

  colors_v = c('Baseline' = 'black', 'Deemed' = 'gray', 'Gross' = 'lightgreen', 'Norm' = 'royalblue')
  labels_v = c('Baseline' = 'Baseline', 'Deemed Savings' = 'gray', 'Gross' = 'Gross Savings',
               'Norm' = 'WN Savings')
  GP = ggplot(data = dat, aes(x = variable,
                              y = use,
                              color = variable,
                              fill = variable,
                              label = rr,
                              ymin = use - 1.96 * sqrt(var_gross),
                              ymax = use + 1.96 * sqrt(var_gross))) +
    theme_minimal() +
    theme(legend.position = 'top') +
    theme(panel.grid = element_blank()) +
    geom_bar(stat = 'identity') +
    geom_errorbar(color = 'black', position = 'dodge', width = 0.5) +
    scale_fill_manual('', values = colors_v,
                      labels = labels_v) +
    scale_color_manual('', values = colors_v,
                       labels = labels_v) +
    scale_x_discrete('') +
    scale_y_continuous(sprintf("%s ('000)", units)) +
    coord_flip() +
    geom_hline(yintercept = 0) +
    facet_wrap(~id, scale = 'free_x', ncol = 3)

  if(label) return(GP + geom_label(fill = 'white', color = 'black', nudge_y = 100))
  GP
}
