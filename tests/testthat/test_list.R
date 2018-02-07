library(sdmpredictors)
library(testthat)

test_that("list_datasets works", {
  l <- list_datasets()
  expect_gte(nrow(l), 4)
})

test_that("list_layers works", {
  l <- list_layers()
  expect_gt(nrow(l), 200)
})

test_that("list_layers_future works", {
  l <- list_layers_future()
  expect_gt(nrow(l), 200)
})

test_that("list_layers_paleo works", {
  l <- list_layers_paleo()
  expect_gt(nrow(l), 200)
})
