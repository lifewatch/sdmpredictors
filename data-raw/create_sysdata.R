source("data-raw/calculate_stats.R")
source("data-raw/bibentries.R")

create_sysdata <- function() {
  inprep <- NULL # c('Freshwater') #"Bio-ORACLE2"
  datasetlist <- read.csv2("data-raw/datasets.csv", stringsAsFactors = FALSE)
  datasetlist <- datasetlist[!(datasetlist$dataset_code %in% inprep),]
  layerlist <- read.csv2("data-raw/layers.csv", stringsAsFactors = FALSE)
  layerlist <- layerlist[!(layerlist$dataset_code %in% inprep),]
  layerlistfuture <- read.csv2("data-raw/layers_future.csv", stringsAsFactors = FALSE)
  layerlistfuture <- layerlistfuture[!(layerlistfuture$dataset_code %in% inprep),]
  layerlistpaleo <- read.csv2("data-raw/layers_paleo.csv", stringsAsFactors = FALSE)
  layerlistpaleo <- layerlistpaleo[!(layerlistpaleo$dataset_code %in% inprep),]
  allclim_layers <- c(layerlist$layer_code, layerlistpaleo$layer_code, layerlistfuture$layer_code)
  layerstats <- get_all_layer_stats(calc=FALSE)
  layerstats <- layerstats[layerstats$layer_code %in% allclim_layers,]
  
  print(allclim_layers[!allclim_layers %in% layerstats$layer_code])
  
  layerscorrelation <- get_all_correlations()
  layerscorrelation <- layerscorrelation[rownames(layerscorrelation) %in% layerlist$layer_code, 
                                         colnames(layerscorrelation)  %in% layerlist$layer_code]
  
  print(layerlist$layer_code[!layerlist$layer_code %in% rownames(layerscorrelation)])
  .data <- list(datasetlist = datasetlist, layerlist = layerlist, layerlistfuture = layerlistfuture,
                layerlistpaleo = layerlistpaleo, layerstats = layerstats, layerscorrelation = layerscorrelation,
                bibentries = bibentries, lnk_bibentry = lnk_bibentry,
                urldata = c("https://www.lifewatch.be/sdmpredictors/", "https://bio-oracle.org/data/"), 
                urlsysdata = c("https://www.lifewatch.be/sdmpredictors/", "https://bio-oracle.org/data/"),
                creation = Sys.time())
  usethis::use_data(.data, internal = TRUE, overwrite = TRUE)
  if(length(file.path(getOption("sdmpredictors_datadir"))) > 0) {
    file.copy("R/sysdata.rda", file.path(getOption("sdmpredictors_datadir"), "sysdata.rda"), overwrite = TRUE)
  }
}
create_sysdata()
