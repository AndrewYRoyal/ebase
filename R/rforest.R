#' Random Forest
#' @import data.table
#' @import mlr
#' @export
rforest <- function(dat, ...) UseMethod('rforest')

#' Hourly random forest
#' @import data.table
#' @import mlr
#' @export
rforest.hourly <- function(dat, ivars, model_options){
  dat <- copy(as.data.table(dat))
  blockFactor <- factor(sort(rep(1:model_options$blocks, length(dat$date))[1:length(dat$date)]))
  weightsV <- tryCatch(dat[[model_options$weights]], error = function(e) NULL)
  regTask <- makeRegrTask(id = 'reg',
                          data = as.data.frame(dat[, (.SD), .SDcols = c('use', model_options$ivars)]),
                          target = 'use',
                          blocking = blockFactor,
                          weights = weightsV)
  paramSpace <- makeParamSet(
    makeDiscreteParam('ntree', values = model_options$ntree))
  ctrl <- makeTuneControlGrid()
  rSampleDesc <- makeResampleDesc('CV', iter = model_options$blocks)
  tuner <- tuneParams(
    learner = 'regr.randomForest',
    task = regTask,
    resampling = rSampleDesc,
    par.set = paramSpace,
    control = ctrl,
    show.info = FALSE)
  xgbLearn <- setHyperPars(
    makeLearner('regr.randomForest'),
    par.vals = tuner$x)
  xgbModel <- train(learner = xgbLearn, ivars = model_options$ivars, task = regTask)
  structure(list(mod = xgbModel), class = c('rforest', 'gboost'))
}
