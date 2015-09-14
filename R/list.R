load_online_sysdata <- function() {
  fname <- "sysdata.rda"
  outfile <- paste0(dirname(tempdir()), "/", fname)
  tryCatch({
    urlroot <- "http://www.phycology.ugent.be/research/sdmpredictors/"
    url <- paste0(urlroot, fname)
    download.file(url, outfile, quiet = TRUE)
  }, finally = {
    if(file.exists(outfile)) {
      load(outfile)
    }
  })
}


read_online_list <- function(fname) {
  outfile <- paste0(dirname(tempdir()), "/", fname, ".csv")
  tryCatch({
    urlroot <- "http://www.phycology.ugent.be/research/sdmpredictors/"
    url <- paste0(urlroot, fname, ".csv")
    download.file(url, outfile, quiet = TRUE)
  }, finally = {
    if(file.exists(outfile)) {
      return(read.csv2(outfile, stringsAsFactors = FALSE))
    }
    return(c())
  })
}

#' Lists the supported datasets
#' 
#' \code{list_datasets} returns information on the supported datasets.
#' 
#' @usage
#' list_datasets(terrestrial = TRUE, marine = TRUE)
#' 
#' @param terrestrial logical. When \code{TRUE}, then datasets 
#' that only have terrestrial data (seamasked) are returned.
#' @param marine logical. When \code{TRUE}, then datasets
#' that only have marine data (landmasked) are returned.
#' 
#' By default it returns all datasets, when both marine and terrestrial are 
#' \code{FALSE} then only datasets without land- nor seamasks are returned.
#' 
#' @return A dataframe with information on the supported datasets.
#' 
#' @examples
#' list_datasets()
#' list_datasets(marine=FALSE)
#' list_datasets(terrestrial=FALSE)
#' 
#' @export
#' @seealso \code{\link{list_layers}}
list_datasets <- function(terrestrial = TRUE, marine = TRUE) {
  load_online_sysdata()
  # reading order: online -> temp -> sysdata
  data <- read_online_list("datasets")
  data <- rbind(data, .datasetlist)
  data <- data[!duplicated(data$dataset_code),]
  
  if(!terrestrial) {
    data <- subset(data, !terrestrial)
  }
  if(!marine) {
    data <- subset(data, !marine)
  }
  return(data)  
}

#' List the layers provided by one or more datasets
#' 
#' \code{list_layers} returns information on the 
#' layers of one or more datasets.
#' 
#' @usage
#' list_layers(datasets=c(), terrestrial = TRUE, marine = TRUE)
#' 
#' @param datasets character vector. Code of the datasets.
#' #' @param terrestrial logical. When \code{TRUE}, then datasets 
#' that only have terrestrial data (seamasked) are returned.
#' @param marine logical. When \code{TRUE}, then datasets
#' that only have marine data (landmasked) are returned.
#' 
#' By default it returns all layers from all datasets, when both marine and terrestrial are 
#' \code{FALSE} then only datasets without land- nor seamasks are returned.
#' 
#' @return A dataframe with information on the supported datasets.
#' 
#' @examples
#' list_layers()
#' list_layers("WorldClim")
#' list_layers("Bio-ORACLE")
#' list_layers(c("Bio-ORACLE","MARSPEC"))
#' list_layers(marine=FALSE)
#' list_layers(terrestrial=FALSE)
#' 
#' @export
#' @seealso \code{\link{list_datasets}}, \code{\link{load_layers}}
list_layers <- function(datasets=c(), terrestrial = TRUE, marine = TRUE) {
  
  # reading order: online -> temp -> sysdata
  data <- read_online_list("layers")
  data <- rbind(data, .layerlist)
  data <- data[!duplicated(data$layer_code),]
  
  if(!terrestrial) {
    data <- subset(data, !terrestrial)
  }
  if(!marine) {
    data <- subset(data, !marine)
  }
  if(is.data.frame(datasets)) {
    datasets <- datasets$dataset_code
  }
  if (length(datasets) > 0) {
    data <- subset(data, dataset_code %in% datasets)
  }
  return(data)
}

