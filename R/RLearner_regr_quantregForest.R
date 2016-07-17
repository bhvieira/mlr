#' @export
makeRLearner.regr.quantregForest = function() {
  makeRLearnerRegr(
    cl = "regr.quantregForest",
    package = "quantregForest",
    par.set = makeParamSet(
      makeIntegerLearnerParam(id = "ntree", default = 500L, lower = 1L),
      makeIntegerLearnerParam(id = "ntree.for.se", default = 100L, lower = 1L),
      makeDiscreteLearnerParam(id = "se.method", default = "jackknife",
                               values = c("bootstrap", "jackknife",  "sd"),
                               requires = quote(se.method %in% c("jackknife") && keep.inbag == TRUE)),
      makeIntegerLearnerParam(id = "se.boot", default = 50L, lower = 1L),
      makeIntegerLearnerParam(id = "mtry", lower = 1L),
      makeLogicalLearnerParam(id = "replace", default = TRUE),
      makeIntegerLearnerParam(id = "sampsize", lower = 1L),
      makeIntegerLearnerParam(id = "nodesize", default = 5L, lower = 1L),
      makeIntegerLearnerParam(id = "maxnodes", lower = 1L),
      makeLogicalLearnerParam(id = "importance", default = FALSE),
      makeLogicalLearnerParam(id = "localImp", default = FALSE),
      makeLogicalLearnerParam(id = "do.trace", default = FALSE, tunable = FALSE),
      makeLogicalLearnerParam(id = "keep.inbag", default = FALSE, tunable = FALSE),

      makeIntegerLearnerParam(id = "nthreads", default = 1L, lower = 1L)
    ),
    properties = c("numerics", "factors", "ordered", "se"),
    name = "Random Forest",
    short.name = "qrf",
    note = "qrf offers all functionalities from rf. See `?regr.randomForest` for information about se estimation. Note that the qrf can freeze the R process if trained on a task with 1 feature which is constant. This can happen in feature forward selection, also due to resampling, and you need to remove such features with removeConstantFeatures."
  )
}

#' @export
trainLearner.regr.quantregForest = function(.learner, .task, .subset, .weights = NULL, ...) {
  data = getTaskData(.task, .subset, target.extra = TRUE)
  m = quantregForest::quantregForest(x = data[["data"]], y = data[["target"]], ...)

  return(m)
}


#' @export
predictLearner.regr.quantregForest = function(.learner, .model, .newdata, ...) {
  predict(.model$learner.model, newdata = .newdata, what = 0.5, ...)
}