#' Gradient Boost
#' @import data.table
#' @import mlr
#' @export
gboost <- function(dat, ...) UseMethod('gboost')

#' Hourly Gradient Boost
#' @import data.table
#' @import mlr
#' @export
gboost.hourly <- function(dat, model_options){
  configureMlr(on.error.dump = FALSE)
  dat <- copy(dat)
  blockFactor <- factor(sort(rep(1:model_options$blocks, length(dat$date))[1:length(dat$date)]))
  if(model_options$block_on_week) blockFactor <- factor(strftime(dat$date, '%U'))
  weightsV <- tryCatch(dat[[model_options$weights]], error = function(e) NULL)
  regTask <- makeRegrTask(id = 'reg',
                          data = dat[, (.SD), .SDcols = c('use', model_options$ivars)],
                          target = 'use',
                          blocking = blockFactor,
                          weights = weightsV)
  paramSpace <- makeParamSet(
    makeDiscreteParam('max_depth', values = model_options$max_depth),
    makeDiscreteParam('nrounds', values = model_options$nrounds),
    makeDiscreteParam('early_stopping_rounds', values = model_options$early_stopping_rounds),
    makeDiscreteParam('eta', values = model_options$eta))
  ctrl <- makeTuneControlGrid()
  rSampleDesc <- makeResampleDesc('CV', iter = model_options$blocks)
  tuner <- tuneParams(
    learner = 'regr.xgboost',
    task = regTask,
    resampling = rSampleDesc,
    par.set = paramSpace,
    control = ctrl,
    show.info = FALSE)

  xgbLearn <- setHyperPars(
    makeLearner('regr.xgboost', verbose = 0),
    par.vals = tuner$x)
  xgbModel <- train(learner = xgbLearn, task = regTask)
  structure(
    list(mod = xgbModel, model_type = 'gboost', ivars = model_options$ivars, weighted = !is.null(model_options$weights)),
    class = 'gboost')
}


#' GBoost Predict
#' @import data.table
#' @import mlr
#' @export
predict.gboost <- function(mod, dat){
  predDT <- copy(dat)
  suppressWarnings({
    predDT[, pUse:= predict(mod$mod, newdata = (dat[, (mod$ivars), with = FALSE]))$data$response]
  })
  predDT
}

