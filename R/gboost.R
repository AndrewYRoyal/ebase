#' Gradient Boost
#' @import data.table
#' @import mlr
#' @export
gboost <- function(dat, ...) UseMethod('gboost')

#' Hourly Gradient Boost
#' @import data.table
#' @import mlr
#' @export
gboost.hourly <- function(dat, ivars, pSet){
  dat <- copy(dat)
  blockFactor <- factor(sort(rep(1:pSet$blocks, length(dat$date))[1:length(dat$date)]))
  weightsV <- tryCatch(dat[[pSet$weights]], error = function(e) NULL)
  regTask <- makeRegrTask(id = 'reg',
                          data = dat[, (.SD), .SDcols = c('use', ivars)],
                          target = 'use',
                          blocking = blockFactor,
                          weights = weightsV)
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
  structure(
    list(mod = xgbModel, model_type = 'gboost', weigthed = !is.null(pSet$weights)),
    class = 'gboost')
}


#' GBoost Predict
#' @import data.table
#' @import mlr
#' @export
predict.gboost <- function(mod, dat, ivars){
  predDT <- copy(dat)
  suppressWarnings({
    predDT[, pUse:= predict(mod$mod, newdata = (dat[, (ivars), with = FALSE]))$data$response]
  })

  predDT
}

