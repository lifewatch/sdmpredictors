library(sdmpredictors)

context("Get sysdata")

test_that("get_sysdata has no unnamed list entries", {
  sysdata <- sdmpredictors:::get_sysdata()
  expect_false(any(names(sysdata) == ""))
})

test_that("get_sysdata returns all expected entries", {
  expect_notnull <- function(x) {
    expect_false(is.null(x)) 
  }
  sysdata <- sdmpredictors:::get_sysdata()
  expect_notnull(sysdata$datasetlist)
  expect_notnull(sysdata$layerlist)
  expect_notnull(sysdata$layerstats)
  expect_notnull(sysdata$layerscorrelation)
  expect_equal(length(names(sysdata)), 4)
})