context("regr_quantregForest")

test_that("regr_quantregForest", {
  requirePackagesOrSkip("quantregForest", default.method = "load")

  parset.list = list(
    list(),
    list(ntree = 5, mtry = 2),
    list(ntree = 5, mtry = 4),
    list(bias.corr = TRUE),
	list(nthreads = 1L),
	list(proximity = TRUE, oob.prox = TRUE)
    list(nPerm = 3)
  )

  old.predicts.list = list()

  for (i in 1:length(parset.list)) {
    parset = parset.list[[i]]
    pars = list(formula = regr.formula, data = regr.train)
    pars = c(pars, parset)
    set.seed(getOption("mlr.debug.seed"))
    m = do.call(quantregForest::quantregForest, pars)
    set.seed(getOption("mlr.debug.seed"))
    p = predict(m, newdata = regr.test, type = "response")
    old.predicts.list[[i]] = p
  }

  testSimpleParsets("regr.quantregForest", regr.df, regr.target,
    regr.train.inds, old.predicts.list, parset.list)

  tt = quantregForest::quantregForest

  testCVParsets("regr.quantregForest", regr.df, regr.target, tune.train = tt, parset.list = parset.list)
})