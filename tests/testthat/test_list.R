library(sdmpredictors)

context("List datasets/layers")

data_raw_file <- function(fname) {
  d <- "../../data-raw/"
  if(!dir.exists(d)) {
    skip("data-raw not found")
  }
  paste0(d, fname)
}

test_that("list_datasets with defaults params returns correct rows", {
  df <- list_datasets()
  expect_false(is.factor(df$dataset_code))
  expect_more_than(nrow(df),0)
  expect_more_than(ncol(df),5)
  expect_true(any(df$terrestrial))
  expect_true(any(df$marine))
  expect_true("citation" %in% colnames(df))
})

test_that("list_datasets type filtering works", {
  df <- list_datasets(terrestrial = FALSE)
  
  expect_more_than(nrow(df),0)
  expect_more_than(nrow(df[df$terrestrial == FALSE,]), 0)
  
  df <- list_datasets(marine = FALSE)
  expect_more_than(nrow(df),0)
  expect_more_than(nrow(df[df$marine == FALSE,]), 0)
})

test_that("list_datasets result is same as datasets.csv", {
  skip_on_cran()
  original <- read.csv2(data_raw_file("datasets.csv"), stringsAsFactors = FALSE)
  df <- list_datasets()
  expect_equal(nrow(df),nrow(original))
  expect_equal(df, original)
})

test_that("list_layers without params returns correct rows", {
  df <- list_layers()
  expect_false(is.factor(df$dataset_code))
  expect_false(is.factor(df$layer_code))
  expect_more_than(nrow(df),0)
  expect_more_than(ncol(df),5)
  expect_true("terrestrial" %in% colnames(df))
  expect_true("marine" %in% colnames(df))
  
})

test_that("list_layers dataset filtering works", {
  filter_test <- function(filter) {
    df <- list_layers(filter)
    expect_more_than(nrow(df),0)
    expect_true(all(df$dataset_code %in% filter))
  }
  filter_test("WorldClim")
  filter_test("Bio-ORACLE")
  filter_test(c("Bio-ORACLE","MARSPEC"))
})

test_that("list_layers type filtering works", {
  df <- list_layers(terrestrial = FALSE)
  expect_more_than(nrow(df),0)
  expect_more_than(nrow(df[df$terrestrial == FALSE,]), 0)
  
  df <- list_layers(marine = FALSE)
  expect_more_than(nrow(df),0)
  expect_more_than(nrow(df[df$marine == FALSE,]), 0)
})

test_that("list_layers result is same as layers.csv", {
  skip_on_cran()
  original <- read.csv2(data_raw_file("layers.csv"), stringsAsFactors = FALSE)
  df <- list_layers()
  expect_equal(nrow(df),nrow(original))
  expect_equal(df, original)
})