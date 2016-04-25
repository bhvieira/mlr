#Trims an ill-conditioned matrix. I added it because `classif.lssvm` suffers a lot if strong colinearity is present.
#The default `tolval` should be enough to avoid numeric erros, but it can be tuned.
#Also cool to have akin to `findLinearCombos` in `caret`.
makePreprocWrapperTrim = function(learner, trim = TRUE, tolval = 10*.Machine$double.eps) {
  requirePackages("subselect", why = "makePreprocWrapperTrim", default.method = "load")
  trainfun = function(data, target, args = list(trim, tolval)) {
  ## Identify numerical features
  cns = colnames(data)
  nums = setdiff(cns[sapply(data, is.numeric)], target)
  ## Extract numerical features from the data set and call trim.matrix
  x = as.matrix(data[, nums, drop = FALSE])
  if(args$trim)
  t = subselect::trim.matrix(cor(x), tolval = args$tolval)$names.discarded
  x = x[, setdiff(colnames(x),t)]

  ## Store the trimming parameters in control
  ## These are needed to preprocess the data before prediction
  control = args
  if (is.logical(control$trim) && control$trim)
    control$trim = t
  ## Recombine the data
  data = data[, setdiff(cns, nums), drop = FALSE]
  data = cbind(data, as.data.frame(x))
  return(list(data = data, control = control))
} 
  predictfun = function(data, target, args, control) {
  ## Identify numerical features
  cns = colnames(data)
  nums = cns[sapply(data, is.numeric)]
  ## Extract numerical features from the data set and call trim.matrix
  x = as.matrix(data[, nums, drop = FALSE])
  x = x[, setdiff(colnames(x),control$trim)]
  ## Recombine the data
  data = data[, setdiff(cns, nums), drop = FALSE]  
  data = cbind(data, as.data.frame(x))
  return(data)
}
  makePreprocWrapper(
    learner,
    train = trainfun,
    predict = predictfun,
    par.set = makeParamSet(
      makeLogicalLearnerParam("trim"),
      makeNumericLearnerParam("tolval")
    ),
    par.vals = list(trim = TRUE, tolval = 10*.Machine$double.eps)
  )
}
