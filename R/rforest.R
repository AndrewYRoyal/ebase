#' Random Forest
#' @import data.table
#' @import mlr
#' @export
rforest <- function(dat, ...) UseMethod('rforest')

#' Hourly random forest
#' @import data.table
#' @import mlr
#' @export
rforest.hourly <- function(dat, ivars, pSet){
  dat <- copy(as.data.table(dat))
  blockFactor <- factor(sort(rep(1:pSet$blocks, length(dat$date))[1:length(dat$date)]))
  weightsV <- tryCatch(dat[[pSet$weights]], error = function(e) NULL)
  regTask <- makeRegrTask(id = 'reg',
                          data = as.data.frame(dat[, (.SD), .SDcols = c('use', ivars)]),
                          target = 'use',
                          blocking = blockFactor,
                          weights = weightsV)
  paramSpace <- makeParamSet(
    makeDiscreteParam('ntree', values = pSet$ntree))
  ctrl <- makeTuneControlGrid()
  rSampleDesc <- makeResampleDesc('CV', iter = pSet$blocks)
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
  xgbModel <- train(learner = xgbLearn, task = regTask)
  structure(list(mod = xgbModel), class = c('rforest', 'gboost'))
}