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
  data <- get_sysdata()$datasetlist
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
#' list_layers(datasets=c(), terrestrial = TRUE, marine = TRUE, monthly = TRUE)
#' 
#' @param datasets character vector. Code of the datasets.
#' #' @param terrestrial logical. When \code{TRUE} (default), then datasets 
#' that only have terrestrial data (seamasked) are returned.
#' @param marine logical. When \code{TRUE} (default), then datasets
#' that only have marine data (landmasked) are returned.
#' @param monthly logical. When \code{FALSE}, then no monthly layers are returned. 
#' All annual and monthly layers are returned by default.
#' 
#' By default it returns all layers from all datasets, when both marine and terrestrial are 
#' \code{FALSE} then only datasets without land- nor seamasks are returned.
#' 
#' @return A dataframe with information on the supported datasets.
#' 
#' @examples
#' # list the first 5 layers
#' list_layers()[1:5,]
#' # list the layercodes all monthly layers from WorldClim
#' worldclim <- list_layers("WorldClim")
#' worldclim[!is.na(worldclim$month),]$layer_code
#' # list layer codes for Bio-ORACLE and MARSPEC
#' list_layers(c("Bio-ORACLE","MARSPEC"))$layer_code
#' # list the first 5 terrestrial layers
#' list_layers(marine=FALSE)[1:5,]
#' # list the first 5 marine layers
#' list_layers(terrestrial=FALSE)[1:5,]
#' # list all annual MARSPEC layers (remove monthly layers)
#' list_layers("MARSPEC", monthly = FALSE)
#' @export
#' @seealso \code{\link{list_datasets}}, \code{\link{load_layers}}
list_layers <- function(datasets=c(), terrestrial = TRUE, marine = TRUE, monthly = TRUE) {
  data <- get_sysdata()$layerlist
  
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
    data <- data[data$dataset_code %in% datasets,]
  }
  if (!monthly) {
    data <- data[is.na(data$month),]
  }
  return(data)
}

