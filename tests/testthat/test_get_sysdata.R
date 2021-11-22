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
  expect_notnull(sysdata$layerlistfuture)
  expect_notnull(sysdata$layerlistpaleo)
  expect_notnull(sysdata$layerstats)
  expect_notnull(sysdata$layerscorrelation)
  expect_notnull(sysdata$urldata)
  expect_notnull(sysdata$urlsysdata)
  expect_notnull(sysdata$bibentries)
  expect_notnull(sysdata$creation)
  expect_gt(length(names(sysdata)), 9)
})
