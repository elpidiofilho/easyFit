library(easyFit)
library(testthat)
library(caret)
library(randomForest)

check_rfe <- function() {
  y = iris[,5]
  x= iris[,1:4]
  xy = data.frame(Species = y, x)
  fit.rfe = recursive_feature_elimination(xy, size = c(2,5, seeds = 313), verbose = FALSE, cpu_cores = 0)
  #print(fit.rfe$optVariables)
  return(as.character(fit.rfe$optVariables))
}

check_clean_names <- function() {
  d = data.frame(iris)
  d1 = clean_names(d)
  return(names(d1))
}

check_classification <- function() {

  y = iris[,5]
  x= iris[,1:4]
  xy = data.frame(Species = y, x)
  fit = classification(xy, classifier = "rf", metric = 'Kappa', tune_length = 3, cpu_cores = 0,
                       seeds = 313, verbose = FALSE)
  return(fit)
}

###

test_that("Recursive feature elimination selected predictors", {
  v = check_rfe() %in% c("Petal.Width", "Petal.Length")
  expect_equal(sum(v),2)
  })


test_that("Clean names of dataframe columns", {
  expect_equal(check_clean_names()  , c("sepal.length","sepal.width","petal.length","petal.width","species"))
})

test_that("Classification kappa", {
  f = check_classification()
  bt = f$bestTune[[1]]
  v = f$results$Kappa[bt]
  check = 0.94
  expect_equal(v, check)
})
