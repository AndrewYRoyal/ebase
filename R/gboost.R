#' Gradient Boost
#' @import data.table
#' @import mlr
#' @export
gboost <- function(x, pSet) UseMethod('gboost')

#' Hourly Gradient Boost
#' @import data.table
#' @import mlr
#' @export
gboost.hourly <- function(x, pSet){
  dat <- copy(x$dat[['baseline']])
  t <- length(dat$date)
  blockFactor <- factor(sort(rep(1:pSet$blocks, t)[1:t]))
  #if(is.null(dat$w)) dat$wt <- 1
  regTask <- makeRegrTask(id = 'reg',
                          data = as.data.frame(
                            dat[period == 'baseline',
                                intersect(names(dat), c('use', 'temp', 'tow', 'mm')), with = FALSE]),
                          target = 'use',
                          #weights = dat[period == 'baseline', wt],
                          blocking = blockFactor)
  paramSpace <- makeParamSet(
    makeDiscreteParam('max_depth', values = pSet$max_depth),
    makeDiscreteParam('nrounds', values = pSet$nrounds),
    makeDiscreteParam('early_stopping_rounds', values = pSet$early_stopping_rounds),
    makeDiscreteParam('eta', values = pSet$eta))
  ctrl <- makeTuneControlGrid()
  rSampleDesc <- makeResampleDesc('CV', iter = pSet$blocks)
  tuner <- tuneParams(
    learner = 'regr.xgboost',
    task = regTask,
    resampling = rSampleDesc,
    par.set = paramSpace,
    control = ctrl,
    show.info = FALSE)
  xgbLearn <- setHyperPars(
    makeLearner('regr.xgboost', verbose = 0, nthread = 8),
    par.vals = tuner$x)
  xgbModel <- train(learner = xgbLearn, task = regTask)
  structure(list(mod = xgbModel), class = 'gboost')
}
