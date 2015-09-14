source("data-raw/calculate_stats.R")



create_sysdata <- function() {
  .datasetlist <- read.csv2("data-raw/datasets.csv", stringsAsFactors = FALSE)
  .layerlist <- read.csv2("data-raw/layers.csv", stringsAsFactors = FALSE)
  .layerstats <- get_all_layer_stats(calc=FALSE)
  .layerscorrelation <- get_all_correlations()
  devtools::use_data(.datasetlist, .layerlist, .layerstats, .layerscorrelation,
                     internal = TRUE, overwrite = TRUE)
}
create_sysdata()