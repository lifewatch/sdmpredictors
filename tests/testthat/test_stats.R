library(sdmpredictors)

context("Statistics")

test_that("layer_stats without args returns all layers", {
  layers <- list_layers()
  layers <- layers[layers$layer_code != "WC_TODO",]
  stats <- layer_stats()
  expect_equal(sum(!is.na(layers$cellsize_equalarea)), nrow(stats))
  expect_true(all(stats$layer_code %in% layers$layer_code))
})

test_that("layer_stats with one or more existing layercodes works", {
  stats <- layer_stats(data.frame(layer_code="BO_calcite"))
  expect_equal(nrow(stats), 1)
  expect_equal(stats$layer_code, "BO_calcite")
  
  stats <- layer_stats(c("BO_calcite","MS_bathy_5m"))
  expect_equal(nrow(stats), 2)
  expect_true(all(stats$layer_code %in% c("BO_calcite", "MS_bathy_5m")))
})

test_that("layer_stats with non existing layercodes generates a warning", {
  skip_on_cran()
  
  expect_warning(layer_stats("blabla"), "'blabla'")
  expect_warning(layer_stats(c("BO_calcite", "blabla")), "'blabla'")
  expect_equal(nrow(layer_stats(c("BO_calcite", "blabla"))), 1)
  expect_warning(layer_stats(c("blibli", "blabla")), "'blibli', 'blabla'")
})

test_that("layers_correlation without args returns correlations for all layers and their quadratic", {
  ##layers_correlation(layercodes = c(), include_quadratic = TRUE)
  layers <- list_layers()
  corr <- layers_correlation()
  expect_equal(2*nrow(layers), nrow(corr))
  expect_equal(2*nrow(layers), ncol(corr))
  print(iconv(colnames(corr)[NROW(corr)], toRaw = T)[[1]])
  print(colnames(corr)[NROW(corr)])
  print(iconv(paste0(layers$layer_code, "\u00B2")[NROW(layers)], toRaw = T)[[1]])
  expect_true(all(layers$layer_code %in% colnames(corr)))
  expect_true(all(paste0(layers$layer_code, "\u00B2") %in% colnames(corr)))
  expect_true(all(sapply(paste0(layers$layer_code, "\u00B2"), function(l) any(grepl(l, colnames(corr), fixed = T)))))
  expect_true(all(sapply(paste0(layers$layer_code, "\u00B2"), function(l) any(grepl(l, colnames(corr))))))
  expect_true(all(rownames(corr) %in% c(layers$layer_code, paste0(layers$layer_code, "\u00B2"))))
})

test_that("layers_correlation with one or more existing layercodes works and quadratic", {
  corr <- layers_correlation("BO_calcite", TRUE)
  expect_equal(nrow(corr), 2)
  expect_true(all(c("BO_calcite","BO_calcite\u00B2") %in% colnames(corr)))
  
  corr <- layers_correlation(c("BO_calcite","MS_bathy_5m"), TRUE)
  expect_equal(nrow(corr), 4)
  expect_true(all(c("BO_calcite", "BO_calcite\u00B2", "MS_bathy_5m", "MS_bathy_5m\u00B2") %in% colnames(corr)))
  
  corr <- layers_correlation(data.frame(layer_code="BO_calcite"), FALSE)
  expect_equal(nrow(corr), 1)
  expect_equal("BO_calcite", colnames(corr))
  
  corr <- layers_correlation(c("BO_calcite","MS_bathy_5m"), FALSE)
  expect_equal(nrow(corr), 2)
  expect_true(all(c("BO_calcite", "MS_bathy_5m") %in% colnames(corr)))
})

test_that("layers_correlation with non existing layercodes generates a warning", {
  skip_on_cran()
  skip_on_travis()
  expect_warning(layers_correlation("abcd"), "'abcd'")
  expect_warning(layers_correlation("blabla", TRUE), "blabla\U00b2")
  expect_warning(layers_correlation("blabla", FALSE), "blabla")
  expect_warning(layers_correlation(c("BO_calcite", "blabla")), "'blabla'")
  expect_equal(colnames(layers_correlation(c("BO_calcite", "blabla"))), c("BO_calcite", "BO_calcite\u00B2"))
  expect_warning(layers_correlation(c("BO_calcite", "blabla"), FALSE), "'blabla'")
  expect_equal(colnames(layers_correlation(c("BO_calcite", "blabla"), FALSE)), "BO_calcite")
  expect_equal(nrow(layers_correlation(c("BO_calcite", "blabla"))), 2)
  expect_equal(nrow(layers_correlation(c("BO_calcite", "blabla"), FALSE)), 1)
  expect_warning(layers_correlation(c("blibli", "blabla")), "'blibli', 'blabla'")
})

expect_group <- function(actual, expected) {
  expect_equal(length(actual), length(expected))
  for (i in 1:length(actual)) {
    expect_true(setequal(actual[[i]], expected[[i]]))
  }
}

test_that("correlation_groups return correct correlation groups", {
  layers_correlation <- data.frame(a=c(1), row.names=c("a"))
  groups <- correlation_groups(layers_correlation)
  expect_group(actual=groups, expected=list("a"))

  layers_correlation <- data.frame(a=c(1,0.8), b=c(0.8,1), row.names=c("a","b"))
  groups <- correlation_groups(layers_correlation)
  expect_group(groups, list(c("a", "b")))
  
  layers_correlation <- data.frame(a=c(1,0.4), b=c(0.4,1), row.names=c("a","b"))
  groups <- correlation_groups(layers_correlation)
  expect_group(groups, list("a", "b"))
  
  layers_correlation <- data.frame(a=c(1,0.4,0.6), b=c(0.4,1,0.3), c=c(0.6,0.3,1), row.names=c("a","b","c"))
  groups <- correlation_groups(layers_correlation)
  expect_group(groups, list("a", "b", "c"))
  groups <- correlation_groups(layers_correlation, max_correlation = 0.5)
  expect_group(groups, list(c("a","c"), "b"))
  groups <- correlation_groups(layers_correlation, max_correlation = 0.35)
  expect_group(groups, list(c("a","b","c")))
  
  # quadratic should be removed but be accounted for
  layers_correlation <- data.frame(a=c(1,0.9,0.4), a2=c(0.9,1,0.8), b=c(0.4,0.8,1), row.names=c("a","a\u00B2","b"))
  colnames(layers_correlation) <- c("a","a\u00B2","b")
  groups <- correlation_groups(layers_correlation)
  expect_group(groups, list(c("a", "b")))
  groups <- correlation_groups(layers_correlation, max_correlation = 0.0)
  expect_group(groups, list(c("a", "b")))
  groups <- correlation_groups(layers_correlation, max_correlation = 0.85)
  expect_group(groups, list("a", "b"))
})

test_that("calc_stats returns stats", {
  s <- sdmpredictors:::calc_stats("mini_raster", raster(matrix(1:100, nrow=10, ncol=10)))
  expect_true(ncol(s) >= 11)
  expect_equal(nrow(s), 1)
})