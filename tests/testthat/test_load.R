library(sdmpredictors)
library(raster)

test_dir <- file.path(tempdir(), "sdmpredictors")
options(sdmpredictors_datadir = test_dir)

check_skip <- function() {
  # skip("skip today")
  skip_on_cran()
  skip_on_ci()
}

context("Load layers")

setup <- function() {
  load_tmp_dir <- file.path(tempdir(),"tmp_load")
  if (dir.exists(load_tmp_dir)) {
    unlink(load_tmp_dir, recursive=TRUE)
  }
  dir.create(load_tmp_dir)
  normalizePath(load_tmp_dir, winslash = "/", mustWork = TRUE)
}
load_tmp_dir <- setup()

load_BO_ph_test <- function(asdataframe=FALSE, rasterstack=TRUE, equalarea = F) {
  check_skip()
  if (asdataframe) { 
    layercodes <- data.frame(layer_code="BO_ph", stringsAsFactors = FALSE)
  } else {
    layercodes <- "BO_ph"
  }
  rs <- load_layers(layercodes, datadir = load_tmp_dir, equalarea = equalarea, rasterstack = rasterstack)
  if(!rasterstack) {
    rs <- rs[[1]]
  }
  expect_false(is.null(rs))
  expect_equal(nlayers(rs), 1)
  
  expect_equal(names(rs),c("BO_ph"))
  if(equalarea) {
    expect_equal(nrow(rs), 2108)
    print(rs@crs@projargs)
    print(sdmpredictors::equalareaproj@projargs)
    expect_equal(rs@crs@projargs, sdmpredictors::equalareaproj@projargs)
  } else {
    expect_equal(nrow(rs), 2160)
    expect_equal(rs@crs, lonlatproj)
  }
}
test_that("load_layer for one not previously downloaded layercode works", {
  load_BO_ph_test()
})
test_that("load_layer for a previously downloaded layer works", {
  load_BO_ph_test(asdataframe=TRUE, rasterstack=FALSE)
})
test_that("load_layer equal area layer works", {
  load_BO_ph_test(equalarea = TRUE)
})
test_that("load_layer works with different datadir options", {
  normalize <- function(p) {
    normalizePath(paste0(p,"/"), winslash = "/", mustWork = TRUE)
  }
  rpath <- function(rs) {
    normalize(dirname(raster(rs,1)@file@name))
  }
  skip_on_cran()
  op <- options()
  wd <- getwd()
  on.exit({
    options(op)
    setwd(wd)
  })
  
  load_tmp_dir <- normalize(load_tmp_dir)
  rs <- rpath(load_layers("BO_ph", datadir = load_tmp_dir))
  expect_equal(rs, load_tmp_dir)
  
  load_tmp_dir <- normalize(load_tmp_dir)
  rs <- rpath(load_layers("BO_ph", datadir = paste0(load_tmp_dir, "/")))
  expect_equal(rs, load_tmp_dir)
  
  tmp <- file.path(tempdir(), "sdmpredictors")
  options(sdmpredictors_datadir = tmp)
  rs <- rpath(load_layers("BO_ph"))
  expect_equal(rs, normalize(tmp))
  
  options(sdmpredictors_datadir = NULL)
  testthat::expect_warning(load_layers("BO_ph"))
})

test_that("load_layer for dataframe from list_layers works", {
  check_skip()
  layers <- list_layers()
  layers <- layers[layers$layer_code == "BO_ph",]
  rs <- load_layers(layers, datadir = load_tmp_dir, equalarea = F)
  expect_false(is.null(rs))
  expect_equal(nlayers(rs), 1)
  expect_equal(nrow(rs), 2160)
  expect_equal(ncol(rs), 4320)
  expect_equal(names(rs),c("BO_ph"))
})

load_multiple_test <- function() {
  check_skip()
  rs <- load_layers(c("BO_ph","BO_dissox"), datadir = load_tmp_dir, equalarea = F)
  expect_false(is.null(rs))
  expect_equal(nlayers(rs), 2)
  expect_equal(nrow(rs), 2160)
  expect_equal(ncol(rs), 4320)
  expect_equal(names(rs),c("BO_ph","BO_dissox"))
}
test_that("load_layer for multiple not previously downloaded layers works", {
  load_multiple_test()
})
test_that("load_layer for multiple previously downloaded layers works", {
  load_multiple_test()
})

load_multiple_mixed <- function() {
  check_skip()
  rs <- load_layers(c("BO_ph","MS_biogeo05_dist_shore_5m"), datadir = load_tmp_dir, equalarea = F)
  expect_false(is.null(rs))
  expect_equal(nlayers(rs), 2)
  expect_equal(nrow(rs), 2160)
  expect_equal(ncol(rs), 4320)
  expect_equal(names(rs),c("BO_ph","MS_biogeo05_dist_shore_5m"))
}
test_that("load_layer for multiple mixed not previously downloaded layers works", {
  load_multiple_mixed()
})
test_that("load_layer for multiple mixed previously downloaded layers works", {
  load_multiple_mixed()
})

test_that("load_layer handles special cases", {
  check_skip()
  expect_error(load_layers("blabla"))
  expect_error(load_layers("BO_ph", equalarea = NA))
  expect_error(load_layers(c("BO_ph", "BO_calcite"), equalarea = c(T,F)))
  expect_warning(load_layers(c("BO_ph", "MS_bathy_21kya"), rasterstack = FALSE))
})

test_that("load_layer equal area TRUE/FALSE works", {
  check_skip()
  rs_default <- load_layers("BO_ph", datadir = load_tmp_dir)
  rs_equalarea <- load_layers("BO_ph", datadir = load_tmp_dir, equalarea = TRUE)
  rs_lonlat <- load_layers("BO_ph", datadir = load_tmp_dir, equalarea = FALSE)
  
  is_equalarea <- function(rs) {
    expect_equal(names(rs), c("BO_ph"))
    expect_identical(rs@crs, sdmpredictors::equalareaproj)
  }
  is_lonlat <- function(rs) {
    expect_equal(nrow(rs), 2160)
    expect_equal(ncol(rs), 4320)
    expect_equal(names(rs), c("BO_ph"))
    expect_identical(rs@crs, sdmpredictors::lonlatproj)
  }
  expect_identical(rs_default, rs_lonlat)
  
  is_equalarea(rs_equalarea)
  is_lonlat(rs_default)
  is_lonlat(rs_lonlat)
})

test_that("A sample of 500 layers is available on http://www.lifewatch.be/sdmpredictors/", {
  check_skip()
  url <- c(list_layers()$layer_code, list_layers_future()$layer_code, list_layers_paleo()$layer_code)
  url <- paste0("http://www.lifewatch.be/sdmpredictors/", url, ".tif")
  url <- sample(url, 500)
  for (url_i in url){
      expect_true(RCurl::url.exists(url_i, useragent="https://github.com/lifewatch/sdmpredictors"))
  }
})

unlink(load_tmp_dir, recursive=TRUE)