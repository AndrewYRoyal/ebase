#' PCA Load Classifier
#' @import data.table
#' @export
ebLoadClass = function(dat, interval = c('hour', 'day'), id = 'meterID', na_drop = TRUE) {
  interval = match.arg(interval)
  if(interval == 'hour') {
    dat = dat[, .(use = use / sum(use), 
                  date = date),
              by = .(id = get(id), day = as.Date(date))]
    dat = dat[, .(use = mean(use, na.rm = na_drop)),
              by = .(id, hour(date))]
    dat = dcast(dat, id ~ hour, value.var = 'use')
    model <- prcomp(dat[, -c('id')])
  } else if(interval == 'day') {
    dat = dat[, .(use = sum(use)),
              by = .(id = get(id), date = as.Date(date))]
    dat = dat[, .(use = use / sum(use), date),
              by = .(id, year = year(date))]
    dat = dat[, .(use = mean(use, na.rm = na_drop)),
              by = .(id, day = yday(date))]
    return(dat)
    dat = dcast(dat, id ~ day, value.var = 'use')
    model <- prcomp(dat[, -c('id')])
  }
  components <- cbind(dat[, .(id)], as.data.table(predict(model, newdata = dat)))
  setnames(components, 'id', id)
  dat = melt(dat, id.var = 'id', value.name = 'use', variable.name = interval)
  setnames(dat, 'id', id)
  
  out = list(model = model,
             components = components, 
             dat = dat)
  out_class = sprintf('LC_%s', interval)
  attr(out, 'class') = c(out_class)
  out
}

#' Loadpath Cluster Generic
#' @export
ebCluster = function(x, ...) UseMethod('ebCluster')

#' Hourly Loadpath Clustering
#' @import data.table
#' @export
ebCluster.LC_hour = function(x, k = 4, seed = 13) {
  set.seed(seed)
  components = copy(x$components)
  id = names(x$components)[1]
  components[, Cluster:= kmeans(components[, -c(1)], centers = k, nstart = 20)$cluster]
  order_reset = setNames(c(1:k), components[, .N, by = .(Cluster)][order(-N)]$Cluster)
  components[, Cluster:= as.factor(order_reset[as.character(Cluster)])]
  clusterDict = setNames(components$Cluster, components[[id]])
  dat = x$dat[, .(use = mean(use), .N),
              by = .(hour = as.numeric(hour), Cluster = clusterDict[get(id)])]
  
  colorDict = c('gray90', colorspace::rainbow_hcl(n = k))
  colorDict = setNames(colorDict, 0:k)
  
  out = list(clusterDict = clusterDict,
             colorDict = colorDict, 
             components = components,
             dat = dat)
  setattr(out, 'class', 'lcluster')
  out
}

#' Plot Loadpath Clusters
#' @import ggplot2
#' @export
ebPlot.lcluster <- function(x) {
  plot_cartesian = ggplot(data = x$components, aes(x = PC1, y = PC2, color = Cluster)) +
    theme_minimal(base_size = 18) +
    geom_point() +
    scale_x_continuous('Principal Component 1') +
    scale_y_continuous('Principal Component 2') +
    scale_color_manual(values = x$colorDict)
  hourDict = setNames(c(paste0(c(12, 1:11), 'AM'), paste0(c(12, 1:11), 'PM')), 0:23)
  clusters = levels(x$dat$Cluster)
  load_dat = rbindlist(
    lapply(clusters, function(cl) {
      dat = copy(x$dat)
      dat[, Level:= paste0('Cluster ', cl)]
      dat[, latent_cluster:= Cluster]
      dat[Cluster != cl, Cluster:= '0']
      dat[, Cluster:= factor(Cluster, levels = names(x$colorDict))]
      dat
    })
  )
  plot_load = ggplot(load_dat, 
                     aes(x = as.numeric(hour), 
                         y = 100 * use, 
                         color = Cluster, 
                         linetype = latent_cluster)) +
    theme_minimal(base_size = 16) +
    theme(axis.text.x = element_text(angle = 45)) +
    geom_line(size = 1.5) +
    scale_color_manual(values = x$colorDict) +
    scale_linetype_manual(values = rep(1, 7)) +
    scale_y_continuous('Daily kWh %') +
    scale_x_continuous('', breaks = c(0:23)[c(TRUE, FALSE)], labels = hourDict[c(TRUE, FALSE)]) +
    guides(color = FALSE, linetype = FALSE) +
    facet_wrap(~Level, ncol = 2)
  plot_freq = ggplot(unique(x$dat[, .(Cluster = as.factor(Cluster), N)]), 
                     aes(x = Cluster, y = N, fill = Cluster, label = format(N, big.mark = ','))) + 
    theme_minimal(base_size = 18) +
    theme(axis.text.x = element_blank()) +
    geom_bar(stat = 'identity') +
    scale_fill_manual(values = x$colorDict) +
    geom_label(fill = 'white')
  
  list(plot_cartesian = plot_cartesian,
       plot_load = plot_load,
       plot_freq = plot_freq)
}

#' Plot Hourly PCA
#' @import ggplot2
#' @export
ebPlot.LC_hour = function(x, component = 'PC1', quantiles = 6) {
  id = names(x$dat)[1]
  pcDict = setNames(x$components[, round(100 * ecdf(get(component))(get(component)))],
                    x$components[[id]])
  dat = x$dat[, pc:= cut(pcDict[get(id)], 100 * (0:quantiles / quantiles), include.lowest = TRUE)]
  dat = dat[, .(use = mean(use)), by = .(pc, hour)]
  dat = merge(
    dat,
    data.table(weight = x$model$rotation[, component],
               hour = as.factor(0:23)))
  color_max = max(x$model$rotation)
  color_min = min(x$model$rotation)
  hourDict = setNames(c(paste0(c(12, 1:11), 'AM'), paste0(c(12, 1:11), 'PM')), 0:23)
  ggplot(dat, aes(x = as.numeric(hour), y = 100 * use, color = weight)) +
    theme_bw(base_size = 16) +
    theme(axis.text.x = element_text(angle = 90)) +
    theme(panel.background = element_rect(fill = 'white'),
          panel.grid = element_line(color = 'white')) +
    geom_point(size = 2) +
    geom_line() +
    scale_color_gradient2(low = 'red', mid = 'gray', high = 'darkgreen', midpoint = 0, 
                          limits = c(color_min, color_max)) +
    scale_x_continuous(name = '', breaks = c(0:23)[c(TRUE, FALSE, FALSE)], 
                       labels = hourDict[c(TRUE, FALSE, FALSE)]) +
    scale_y_continuous('Daily kWh %') +
    guides(color = FALSE) +
    facet_wrap(~pc, ncol = 3)
}