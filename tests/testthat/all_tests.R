library(easyFit)
library(devtools)
library(testthat)
library(caret)

check_rfe <- function() {
  y = iris[,5]
  x= iris[,1:4]
  xy = data.frame(Species = y, x)
  fit.rfe = recursive_feature_elimination(xy, size = c(2,5, seeds = 313), verbose = FALSE)
  #print(fit.rfe$optVariables)
  return(as.character(fit.rfe$optVariables))
}

check_clean_names <- function() {
  return(names(clean_names(iris)))
}

check_classification <- function() {
  y = iris[,5]
  x= iris[,1:4]
  xy = data.frame(Species = y, x)
  fit = classification(xy, classifier = "rf", metric = 'Kappa', seeds = 313, verbose = FALSE)
  v = unlist(caret::getTrainPerf(fit)[2])
  names(v) = NULL
  return(v)
}

###

test_that("Recursive feature elimination selected predictors", {
  expect_equal(check_rfe() , c("Petal.Width", "Petal.Length" ))
  })


test_that("Clean names of dataframe columns", {
  expect_equal(check_clean_names()  , c("sepal.length","sepal.width","petal.length","petal.width","species"))
})

test_that("Classification kappa", {
  expect_equal(check_classification(), 0.94)
})
