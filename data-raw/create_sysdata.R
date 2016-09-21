source("data-raw/calculate_stats.R")

create_sysdata <- function() {
  datasetlist <- read.csv2("data-raw/datasets.csv", stringsAsFactors = FALSE)
  layerlist <- read.csv2("data-raw/layers.csv", stringsAsFactors = FALSE)
  layerlistfuture <- read.csv2("data-raw/layers_future.csv", stringsAsFactors = FALSE)
  layerlistpaleo <- read.csv2("data-raw/layers_paleo.csv", stringsAsFactors = FALSE)
  layerstats <- get_all_layer_stats(calc=FALSE)
  layerscorrelation <- get_all_correlations()
  .data <- list(datasetlist = datasetlist, layerlist = layerlist, layerlistfuture = layerlistfuture,
                layerlistpaleo = layerlistpaleo, layerstats = layerstats, layerscorrelation = layerscorrelation,
                urldata = "http://www.phycology.ugent.be/research/sdmpredictors/", 
                urlsysdata = "http://www.phycology.ugent.be/research/sdmpredictors/")
  devtools::use_data(.data, internal = TRUE, overwrite = TRUE)
  
  file.copy("R/sysdata.rda", 
            "\\\\files.ugent.be/swbosch/www/shares/phycology/WWW/research/sdmpredictors/sysdata.rda",
            overwrite = TRUE)
}
create_sysdata()
