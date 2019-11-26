#' Format Savings from Summary Object
#' @import data.table
#' @export
ebSavings = function(x, deemed, groups = NULL, sites_subset = NULL) {
  dat = merge(
    x$Sites$savings[, .(site, Gross = Savings, gross_var = varSavings)],
    x$Sites$metrics[, .(site, Baseline = Annual)])
  dat[, Deemed:= deemed[site]]
  if(!is.null(sites_subset)) dat = dat[site %in% sites_subset]
  cols = c('Gross', 'Baseline', 'Deemed', 'gross_var')
  dat = dat[, lapply(.SD, as.numeric), .SDcols = cols, by = .(id = site)]
  if(!is.null(groups)) dat = dat[, lapply(.SD, sum), .SDcols = cols, by = .(id = groups[id])]
  formatted = dat[, .(id = id,
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

  gross_frm = sprintf('Gross ~ %s  - 1', paste(measures_v, collapse = '+'))
  gross_model = summary(lm(gross_frm, data = dat))
  deemed_frm = sprintf('Deemed ~ %s  - 1', paste(measures_v, collapse = '+'))
  deemed_model = summary(lm(deemed_frm, data = dat))
  if(!is.null(reporting_subset)) measures = reporting_subset
  dat = data.table(id = measures,
                   Gross = gross_model$coefficients[measures, 1],
                   Deemed = deemed_model$coefficients[measures, 1],
                   gross_var = gross_model$coefficients[measures, 2] ^2)
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
ebPlot.savings = function(x, units = 'kWh') {
  dat = melt(x$raw[, -c('gross_var')], id.vars = 'id', value.name = 'use')
  dat = merge(dat,
              x$raw[, .(variable = 'Gross', id, gross_var)],
              by = c('id', 'variable'),
              all.x = TRUE)
  dat[, use:= use / 1e3]
  dat[, gross_var:= gross_var / 1e6]; dat[is.na(gross_var), gross_var:= 0]
  dat[, variable:= factor(variable, levels = c('Baseline', 'Deemed', 'Gross'))]

  colors_v = c('Baseline' = 'black', 'Deemed' = 'gray', 'Gross' = 'lightgreen')
  labels_v = c('Baseline' = 'Baseline', 'Deemed Savings' = 'gray', 'Gross' = 'Gross Savings')
  ggplot(data = dat, aes(x = variable,
                         y = use,
                         color = variable,
                         fill = variable,
                         ymin = use - 1.96 * sqrt(gross_var),
                         ymax = use + 1.96 * sqrt(gross_var))) +
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
}
