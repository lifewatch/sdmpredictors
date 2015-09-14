library(sdmpredictors)
library(raster)

check_skip <- function() {
  skip("skip today")
  skip_on_cran()
}

context("Load layers")

load_tmp_dir <- "../tmp_load"
setup <- function() {
  if (dir.exists(load_tmp_dir)) {
    unlink(load_tmp_dir, recursive=TRUE)
  }
  dir.create(load_tmp_dir)
}
setup()

load_BO_calcite_test <- function() {
  check_skip()
  rs <- load_layers("BO_calcite", datadir = load_tmp_dir, equalarea = F)
  expect_false(is.null(rs))
  expect_equal(nlayers(rs), 1)
  expect_equal(nrow(rs), 2160)
  expect_equal(ncol(rs), 4320)
  expect_equal(names(rs),c("BO_calcite"))
}
test_that("load_layer for one not previously downloaded layercode works", {
  load_BO_calcite_test()
})
test_that("load_layer for a previously downloaded layer works", {
  load_BO_calcite_test()
})
test_that("load_layer for partially downloaded layer works", {
  skip_on_cran()
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
    behrmann <- sp::CRS("+proj=cea +lon_0=0 +lat_ts=30 +x_0=0 +y_0=0 +datum=WGS84 +ellps=WGS84 +units=m +no_defs")
    expect_identical(rs@crs, behrmann)
  }
  is_lonlat <- function(rs) {
    expect_equal(nrow(rs), 2160)
    expect_equal(ncol(rs), 4320)
    expect_equal(names(rs), c("BO_calcite"))
    wgs84 <- sp::CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
    expect_identical(rs@crs, wgs84)
  }
  expect_identical(rs_default, rs_equalarea)
  is_equalarea(rs_default)
  is_equalarea(rs_equalarea)
  is_lonlat(rs_lonlat)
})

unlink(load_tmp_dir, recursive=TRUE)