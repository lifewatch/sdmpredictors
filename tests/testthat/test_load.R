library(sdmpredictors)
library(raster)

check_skip <- function() {
 # skip("skip today")
  skip_on_cran()
}

context("Load layers")

setup <- function() {
  load_tmp_dir <- "../tmp_load/"
  if (dir.exists(load_tmp_dir)) {
    unlink(load_tmp_dir, recursive=TRUE)
  }
  dir.create(load_tmp_dir)
  normalizePath(load_tmp_dir, winslash = "/", mustWork = TRUE)
}
load_tmp_dir <- setup()

load_BO_calcite_test <- function(asdataframe=FALSE, rasterstack=TRUE, equalarea = F) {
  skip_on_cran()
  skip_on_travis()
  layercodes <- ifelse(asdataframe, data.frame(layer_code="BO_calcite"), "BO_calcite")
  rs <- load_layers(layercodes, datadir = load_tmp_dir, equalarea = equalarea)
  expect_false(is.null(rs))
  expect_equal(nlayers(rs), 1)
  
  expect_equal(names(rs),c("BO_calcite"))
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
  load_BO_calcite_test()
})
test_that("load_layer for a previously downloaded layer works", {
  load_BO_calcite_test(asdataframe=TRUE, rasterstack=FALSE)
})
test_that("load_layer equal area layer works", {
  load_BO_calcite_test(equalarea = TRUE)
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
  tryCatch({
    load_tmp_dir <- normalize(load_tmp_dir)
    rs <- rpath(load_layers("BO_calcite", datadir = load_tmp_dir))
    expect_equal(rs, load_tmp_dir)
    
    options(sdmpredictors_datadir = tmpDir())
    rs <- rpath(load_layers("BO_calcite"))
    expect_equal(rs, normalize(tmpDir()))
    
    options(sdmpredictors_datadir = NULL)
    setwd(load_tmp_dir)
    rs <- rpath(load_layers("BO_calcite"))
    default_path <- normalize(file.path(path.expand("~"), "R/sdmpredictors"))
    expect_equal(rs, default_path)
  }, finally = { 
    options(op)
    setwd(wd)
  }) # reset original options
})

test_that("load_layer for partially downloaded layer works", {
  check_skip()
  # del grd
  unlink(paste0(load_tmp_dir,"/", "BO_calcite.grd"), recursive=FALSE)
  load_BO_calcite_test()
  # del gri
  unlink(paste0(load_tmp_dir,"/", "BO_calcite.gri"), recursive=FALSE)
  load_BO_calcite_test()
})

test_that("load_layer for dataframe from list_layers works", {
  check_skip()
  layers <- list_layers()
  layers <- layers[layers$layer_code == "BO_calcite",]
  rs <- load_layers(layers, datadir = load_tmp_dir, equalarea = F)
  expect_false(is.null(rs))
  expect_equal(nlayers(rs), 1)
  expect_equal(nrow(rs), 2160)
  expect_equal(ncol(rs), 4320)
  expect_equal(names(rs),c("BO_calcite"))
})

load_multiple_test <- function() {
  check_skip()
  rs <- load_layers(c("BO_ph","BO_chlomin"), datadir = load_tmp_dir, equalarea = F)
  expect_false(is.null(rs))
  expect_equal(nlayers(rs), 2)
  expect_equal(nrow(rs), 2160)
  expect_equal(ncol(rs), 4320)
  expect_equal(names(rs),c("BO_ph","BO_chlomin"))
}
test_that("load_layer for multiple not previously downloaded layers works", {
  load_multiple_test()
})
test_that("load_layer for multiple previously downloaded layers works", {
  load_multiple_test()
})

load_multiple_mixed <- function() {
  check_skip()
  rs <- load_layers(c("BO_damax","MS_sst07_5m"), datadir = load_tmp_dir, equalarea = F)
  expect_false(is.null(rs))
  expect_equal(nlayers(rs), 2)
  expect_equal(nrow(rs), 2160)
  expect_equal(ncol(rs), 4320)
  expect_equal(names(rs),c("BO_damax","MS_sst07_5m"))
}
test_that("load_layer for multiple mixed not previously downloaded layers works", {
  load_multiple_mixed()
})
test_that("load_layer for multiple mixed previously downloaded layers works", {
  load_multiple_mixed()
})


test_that("load_layer equal area TRUE/FALSE works", {
  check_skip()
  rs_default <- load_layers("BO_calcite", datadir = load_tmp_dir)
  rs_equalarea <- load_layers("BO_calcite", datadir = load_tmp_dir, equalarea = TRUE)
  rs_lonlat <- load_layers("BO_calcite", datadir = load_tmp_dir, equalarea = FALSE)
  
  is_equalarea <- function(rs) {
    expect_equal(names(rs), c("BO_calcite"))
    expect_identical(rs@crs, sdmpredictors::equalareaproj)
  }
  is_lonlat <- function(rs) {
    expect_equal(nrow(rs), 2160)
    expect_equal(ncol(rs), 4320)
    expect_equal(names(rs), c("BO_calcite"))
    expect_identical(rs@crs, sdmpredictors::lonlatproj)
  }
  expect_identical(rs_default, rs_equalarea)
  is_equalarea(rs_default)
  is_equalarea(rs_equalarea)
  is_lonlat(rs_lonlat)
})

unlink(load_tmp_dir, recursive=TRUE)