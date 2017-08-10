library(sdmpredictors)

inprep <- "Bio-ORACLE2"

context("List datasets/layers")

options(sdmpredictors_datadir = file.path(tempdir(), "sdmpredictors"))

data_raw_file <- function(fname) {
  testthat::skip_on_cran()
  d <- "../../data-raw/"
  if(!dir.exists(d)) {
    skip("data-raw not found")
  }
  paste0(d, fname)
}

test_that("list_datasets with defaults params returns correct rows", {
  df <- list_datasets()
  expect_false(is.factor(df$dataset_code))
  expect_gt(nrow(df),0)
  expect_gt(ncol(df),5)
  expect_true(any(df$terrestrial))
  expect_true(any(df$marine))
  expect_true("citation" %in% colnames(df))
})

test_that("list_datasets type filtering works", {
  df <- list_datasets(terrestrial = FALSE)
  
  expect_gt(nrow(df),0)
  expect_gt(nrow(df[df$terrestrial == FALSE,]), 0)
  
  df <- list_datasets(marine = FALSE)
  expect_gt(nrow(df),0)
  expect_gt(nrow(df[df$marine == FALSE,]), 0)
})

test_that("list_datasets result is same as datasets.csv", {
  skip_on_cran()
  original <- read.csv2(data_raw_file("datasets.csv"), stringsAsFactors = FALSE)
  original <- original[!(original$dataset_code %in% inprep),]
  df <- list_datasets()
  expect_false(any(inprep %in% df$dataset_code))
  df <- df[!(df$dataset_code %in% inprep),]
  expect_equal(nrow(df),nrow(original))
  expect_equal(df, original)
})

test_that("list_layers without params returns correct rows", {
  df <- list_layers()
  expect_false(is.factor(df$dataset_code))
  expect_false(is.factor(df$layer_code))
  expect_gt(nrow(df),0)
  expect_gt(ncol(df),5)
  expect_true("terrestrial" %in% colnames(df))
  expect_true("marine" %in% colnames(df))
  
})

test_that("list_layers dataset filtering works", {
  filter_test <- function(filter) {
    df <- list_layers(filter)
    expect_gt(nrow(df),0)
    expect_true(all(df$dataset_code %in% filter))
  }
  filter_test("WorldClim")
  filter_test("Bio-ORACLE")
  filter_test(c("Bio-ORACLE","MARSPEC"))
})

test_that("list_layers type filtering works", {
  df <- list_layers(terrestrial = FALSE)
  expect_gt(nrow(df),0)
  expect_gt(nrow(df[df$terrestrial == FALSE,]), 0)
  
  df <- list_layers(marine = FALSE)
  expect_gt(nrow(df),0)
  expect_gt(nrow(df[df$marine == FALSE,]), 0)
})

test_that("list_layers month filtering works", {
  df <- list_layers(monthly = F)
  expect_gt(nrow(df),0)
  expect_equal(nrow(df[df$month %in% seq(1:12),]), 0)
  
  df <- list_layers(monthly = T)
  expect_gt(nrow(df),0)
  expect_gt(nrow(df[df$month %in% seq(1:12),]), 0)
})

test_that("list_layers version filtering works", {
  df <- list_layers(version = NULL)
  expect_gt(nrow(df),0)
  expect_gt(length(unique(df$version)), 1)
  
  df <- list_layers(version = c(1,2))
  expect_gt(nrow(df),0)
  expect_true(all(df$version %in% c(1,2)))
  
  df <- list_layers(version = c(1))
  expect_gt(nrow(df),0)
  expect_true(all(df$version %in% c(1)))
})

test_that("list_layers result is same as layers.csv", {
  skip_on_cran()
  original <- read.csv2(data_raw_file("layers.csv"), stringsAsFactors = FALSE)
  original <- original[!(original$dataset_code %in% inprep),]
  df <- list_layers()
  expect_false(any(inprep %in% df$dataset_code))
  expect_equal(nrow(df),nrow(original))
  expect_equal(df, original)
})

test_that("list_layers_future filters scenario", {
  df <- list_layers_future(datasets = "Bio-ORACLE", scenario = "B1")
  expect_equal(nrow(df),10)
  expect_true(all(df$scenario == "B1"))
})

test_that("list_layers_future filters year", {
  df <- list_layers_future(datasets = "Bio-ORACLE", year = 2100)
  expect_gt(nrow(df),300)
  expect_true(all(df$year == 2100))
})

test_that("list_layers_future filters general", {
  df <- list_layers_future(terrestrial=FALSE)
  expect_gt(ncol(df),0)
  df <- list_layers_future(marine=FALSE)
  expect_gt(ncol(df),0)
  df <- list_layers_future(monthly=FALSE)
  expect_gt(ncol(df),0)
})

test_that("list_layers_future filters version", {
  df <- list_layers_future(version=1)
  expect_gt(ncol(df),0)
  expect_gt(nrow(df),0)
  df <- list_layers_future(version=2)
  expect_gt(ncol(df),0)
  expect_gt(nrow(df),0)
})

test_that("list_layers_future result is same as layers_future.csv", {
  skip_on_cran()
  original <- read.csv2(data_raw_file("layers_future.csv"), stringsAsFactors = FALSE)
  df <- list_layers_future()
  expect_equal(nrow(df),nrow(original))
  expect_equal(df, original)
})

test_that("get_future_layers returns correct layer code", {
  li <- get_layers_info("BO_sstmax")$current
  l <- get_future_layers(li, "B1", 2100)
  expect_equal(NROW(l), 1)
  l <- get_future_layers(c("BO_salinity", "BO_sstmean"), "B1", 2100)
  expect_equal(NROW(l), 2)
})

test_that("get_future_layers stops with wrong layer code", {
  l <- get_future_layers(c("BO_salinity", "BO_sstmean"), "B1", 2100)
  expect_equal(NROW(l), 2)
  expect_error(get_future_layers(c("BO_salinity", "blabla"), "B1", 2100), "blabla")
  expect_error(get_future_layers("BO_salinity", "scenarioblabla", 2100), "scenarioblabla")
  expect_error(get_future_layers("BO_salinity", "B1", 555), "555")
})


test_that("list_layers_paleo filters model_name", {
  df <- list_layers_paleo(datasets = "MARSPEC", model_name = "21kya_geophysical")
  expect_equal(nrow(df),8)
  expect_true(all(df$model_name == "21kya_geophysical"))
})

test_that("list_layers_paleo filters epoch", {
  df <- list_layers_paleo(datasets = "MARSPEC", epoch = "mid-Holocene")
  expect_equal(nrow(df),10)
  expect_true(all(df$epoch == "mid-Holocene"))
})

test_that("list_layers_paleo filters years_ago", {
  df <- list_layers_paleo(datasets = "MARSPEC", years_ago = 6000)
  expect_equal(nrow(df),10)
  expect_true(all(df$years_ago == 6000))
})

test_that("list_layers_paleo filters general", {
  df <- list_layers_paleo(terrestrial=FALSE)
  expect_gt(ncol(df),0)
  df <- list_layers_paleo(marine=FALSE)
  expect_gt(ncol(df),0)
  df <- list_layers_paleo(monthly=FALSE)
  expect_gt(ncol(df),0)
})

test_that("list_layers_paleo filters version", {
  df <- list_layers_paleo(version=1)
  expect_gt(ncol(df),0)
  expect_gt(nrow(df),0)
})

test_that("list_layers_future result is same as layers_future.csv", {
  skip_on_cran()
  original <- read.csv2(data_raw_file("layers_future.csv"), stringsAsFactors = FALSE)
  df <- list_layers_future()
  expect_equal(nrow(df),nrow(original))
  expect_equal(df, original)
})

test_that("get_paleo_layers returns layer code", {
  l <- get_paleo_layers("MS_bathy_5m", model_name = "21kya_geophysical", epoch = "Last Glacial Maximum", years_ago = 21000)
  expect_equal(NROW(l), 1)
})

test_that("get_paleo_layers stops with wrong layer code", {
  expect_error(get_paleo_layers(c("blabla")), "blabla")
})

test_that("get_layers_info returns info", {
  l <- get_layers_info(c("BO_salinity", "BO_B1_2100_salinity"))
  expect_equal(nrow(l$current), 1)
  expect_equal(nrow(l$future), 1)
  expect_equal(nrow(l$paleo), 0)
  expect_equal(nrow(l$common), 2)
})

