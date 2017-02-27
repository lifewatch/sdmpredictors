source("data-raw/calculate_stats.R")
source("data-raw/bibentries.R")

create_sysdata <- function() {
  datasetlist <- read.csv2("data-raw/datasets.csv", stringsAsFactors = FALSE)
  layerlist <- read.csv2("data-raw/layers.csv", stringsAsFactors = FALSE)
  layerlistfuture <- read.csv2("data-raw/layers_future.csv", stringsAsFactors = FALSE)
  layerlistpaleo <- read.csv2("data-raw/layers_paleo.csv", stringsAsFactors = FALSE)
  layerstats <- get_all_layer_stats(calc=FALSE)
  layerscorrelation <- get_all_correlations()
  .data <- list(datasetlist = datasetlist, layerlist = layerlist, layerlistfuture = layerlistfuture,
                layerlistpaleo = layerlistpaleo, layerstats = layerstats, layerscorrelation = layerscorrelation,
                bibentries = bibentries,
                urldata = "http://sdmpredictors.samuelbosch.com/", 
                urlsysdata = "http://sdmpredictors.samuelbosch.com/")
  devtools::use_data(.data, internal = TRUE, overwrite = TRUE)
  if(!is.null(file.path(getOption("sdmpredictors_datadir")))) {
    file.copy("R/sysdata.rda", file.path(getOption("sdmpredictors_datadir"), "sysdata.rda"), overwrite = TRUE)
  }
  file.copy("R/sysdata.rda", 
            "\\\\files.ugent.be/swbosch/www/shares/phycology/WWW/research/sdmpredictors/sysdata.rda",
            overwrite = TRUE)
  stop("implement upload to VLIZ FTP")
  #RCurl::ftpUpload("R/sysdata.rda", "ftp://User:Password@FTPServer/Destination.html")
  stop("avoid adding the ftp password in github :-)")
}
create_sysdata()
