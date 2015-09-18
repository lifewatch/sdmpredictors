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

test_that("sysdata gets downloaded", {
  fname <- "sysdata.rda"
  outfile <- paste0(dirname(tempdir()), "/", fname)
  if(file.exists(outfile)) {
    file.remove(outfile)
    expect_false(file.exists(outfile))
  }
  sysdata <- sdmpredictors:::get_sysdata()
  expect_true(file.exists(outfile))
})