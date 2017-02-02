#' Lists the supported datasets
#' 
#' \code{list_datasets} returns information on the supported datasets.
#' 
#' @usage list_datasets(terrestrial=TRUE, marine=TRUE)
#'   
#' @param terrestrial logical. When \code{TRUE}, then datasets that only have
#'   terrestrial data (seamasked) are returned.
#' @param marine logical. When \code{TRUE}, then datasets that only have marine
#'   data (landmasked) are returned.
#'   
#' @details By default it returns all datasets, when both \code{marine} and
#'   \code{terrestrial} are \code{FALSE} then only datasets without land- nor
#'   seamasks are returned.
#'   
#' @return A dataframe with information on the supported datasets.
#'   
#' @examples
#' list_datasets()
#' list_datasets(marine=FALSE)
#' list_datasets(terrestrial=FALSE)
#' 
#' @export
#' @seealso \code{\link{list_layers}}, \code{\link{list_layers_future}}, 
#'   \code{\link{list_layers_paleo}}
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

#' List the current climate layers provided by one or more datasets
#' 
#' \code{list_layers} returns information on the layers of one or more datasets.
#' 
#' @usage list_layers(datasets=c(), terrestrial=TRUE, marine=TRUE, monthly=TRUE)
#'   
#' @param datasets character vector. Code of the datasets.
#' @param terrestrial logical. When \code{TRUE} (default), then datasets that 
#'   only have terrestrial data (seamasked) are returned.
#' @param marine logical. When \code{TRUE} (default), then datasets that only 
#'   have marine data (landmasked) are returned.
#' @param monthly logical. When \code{FALSE}, then no monthly layers are 
#'   returned. All annual and monthly layers are returned by default.
#'   
#' @details By default it returns all layers from all datasets, when both marine
#'   and terrestrial are \code{FALSE} then only datasets without land- nor 
#'   seamasks are returned.
#'   
#' @return A dataframe with information on the supported current climate layers.
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
#' @seealso \code{\link{load_layers}}, \code{\link{list_datasets}}, 
#'   \code{\link{list_layers_future}}, \code{\link{list_layers_paleo}}
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

#' List the future climate layers provided by one or more datasets
#' 
#' \code{list_layers_future} returns information on the future climate layers of
#' one or more datasets.
#' 
#' @usage list_layers_future(datasets=c(), scenario=NA, year=NA,
#'   terrestrial=TRUE, marine=TRUE, monthly=TRUE)
#'   
#' @param datasets character vector. Code of the datasets.
#' @param scenario character vector. Climate change scenario, e.g. \code{"B1", 
#'   "A1B", "A2"}.
#' @param year integer. Year for which you want the climate change prediction, 
#'   e.g. \code{2100, 2200}.
#' @param terrestrial logical. When \code{TRUE} (default), then datasets that 
#'   only have terrestrial data (seamasked) are returned.
#' @param marine logical. When \code{TRUE} (default), then datasets that only 
#'   have marine data (landmasked) are returned.
#' @param monthly logical. When \code{FALSE}, then no monthly layers are 
#'   returned. All annual and monthly layers are returned by default.
#'   
#' @details By default it returns all layers from all datasets, when both marine
#'   and terrestrial are \code{FALSE} then only datasets without land- nor 
#'   seamasks are returned.
#'   
#' @return A dataframe with information on the supported future climate layers.
#'   
#' @examples
#' # list the first 5 layers
#' list_layers_future()[1:5,]
#' # list layer codes for Bio-ORACLE with scenario B1 and year 2100
#' list_layers_future("Bio-ORACLE", scenario = "B1", year = 2100)$layer_code
#' @export
#' @seealso \code{\link{list_layers}}, \code{\link{list_layers_paleo}}, 
#'   \code{\link{list_datasets}}, \code{\link{load_layers}}
list_layers_future <- function(datasets=c(), scenario = NA, year = NA, 
                               terrestrial = TRUE, marine = TRUE, monthly = TRUE) {
  data <- get_sysdata()$layerlistfuture
  
  if(!is.na(scenario)) {
    data <- data[data$scenario %in% scenario,]
  }
  if(!is.na(year)) {
    data <- data[data$year %in% year,]
  }
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


#' Get the name of a future climate layer(s) based on the current climate
#' layer(s)
#' 
#' \code{get_future_layers} returns information on the future climate layers for
#' the matching current climate layers.
#' 
#' @usage get_future_layers(current_layer_codes, scenario, year)
#'   
#' @param current_layer_codes character vector. Code(s) of the current climate
#'   layers either as a character vector or as the dataframe provided by
#'   \code{\link{list_layers}}.
#' @param scenario character vector. Climate change scenario, e.g. \code{"B1",
#'   "A1B", "A2"}.
#' @param year integer. Year for which you want the climate change prediction,
#'   e.g. \code{2100, 2200}.
#'   
#' @details Stops with an exception if no matching future climate layer was
#'   found for one or more of the provided current climate layer codes.
#'   
#' @return A dataframe with information on the future layer(s) matching the
#'   provided current layer(s).
#'   
#' @examples
#' future_layers <- get_future_layers(c("BO_salinity", "BO_sstmean"), 
#'                                    scenario = "B1", year = 2100)
#' future_layers$layer_code
#' @export
#' @seealso \code{\link{list_layers_future}}, \code{\link{list_layers}},
#'   \code{\link{load_layers}}
get_future_layers <- function(current_layer_codes, scenario, year) {
  if(is.data.frame(current_layer_codes)) {
    current_layer_codes <- current_layer_codes$layer_code
  }
  data <- get_sysdata()$layerlistfuture
  m <- data$current_layer_code %in% current_layer_codes & data$scenario %in% scenario & data$year %in% year
  data <- data[m,]
  current_found <- current_layer_codes %in% data$current_layer_code
  if(!all(current_found)) {
    stop(paste0("future layer code not found for layercodes: ", 
                paste0(current_layer_codes[!current_found], collapse = ", "), 
                " and scenario = ", scenario,
                " and year = ", year))
  }
  return(data)
}


#' List the future climate layers provided by one or more datasets
#' 
#' \code{list_layers_paleo} returns information on the future climate layers of 
#' one or more datasets.
#' 
#' @usage list_layers_paleo(datasets=c(), model_name=NA, epoch=NA, years_ago=NA,
#'   terrestrial=TRUE, marine=TRUE, monthly=TRUE)
#'   
#' @param datasets character vector. Code of the datasets.
#' @param model_name character vector. Paleo climate model name see the 
#'   \code{model_name} column in the result.
#' @param epoch character vector. Epoch for which you want the paleo layer, e.g.
#'   \code{"mid-Holocene", "Last Glacial Maximum"}.
#' @param years_ago integer. Years for which you want the paleo layer, e.g. 
#'   \code{6000, 21000}.
#' @param terrestrial logical. When \code{TRUE} (default), then datasets that 
#'   only have terrestrial data (seamasked) are returned.
#' @param marine logical. When \code{TRUE} (default), then datasets that only 
#'   have marine data (landmasked) are returned.
#' @param monthly logical. When \code{FALSE}, then no monthly layers are 
#'   returned. All annual and monthly layers are returned by default.
#'   
#' @details By default it returns all layers from all datasets, when both marine
#'   and terrestrial are \code{FALSE} then only datasets without land- nor 
#'   seamasks are returned.
#'   
#' @return A dataframe with information on the supported paleo climate layers.
#'   
#' @examples
#' # list the first 5 layers
#' list_layers_paleo()[1:5,]
#' # list layer codes for MARSPEC for the mid-Holocene
#' list_layers_paleo("MARSPEC", epoch = "mid-Holocene")$layer_code
#' @export
#' @seealso \code{\link{list_layers}}, \code{\link{list_layers_future}}, 
#'   \code{\link{list_datasets}}, \code{\link{load_layers}}
list_layers_paleo <- function(datasets=c(), model_name = NA, epoch = NA, years_ago = NA,
                               terrestrial = TRUE, marine = TRUE, monthly = TRUE) {
  data <- get_sysdata()$layerlistpaleo
  
  if(!is.na(model_name)) {
    data <- data[data$model_name %in% model_name,]
  }
  if(!is.na(epoch)) {
    data <- data[data$epoch %in% epoch,]
  }
  if(!is.na(years_ago)) {
    data <- data[data$years_ago %in% years_ago,]
  }
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

#' Get the name of a paleo climate layer(s) based on the current climate
#' layer(s)
#' 
#' \code{get_paleo_layers} returns information on the future climate layers for
#' the matching current climate layers.
#' 
#' @usage get_paleo_layers(current_layer_codes, model_name = NA, epoch = NA,
#'   years_ago = NA)
#'   
#' @param current_layer_codes character vector. Code(s) of the current climate
#'   layers either as a character vector or as the dataframe provided by
#'   \code{\link{list_layers}}.
#' @param model_name character vector. Paleo climate model name see the
#'   \code{model_name} column in the result from
#'   \code{\link{list_layers_paleo}}.
#' @param epoch character vector. Epoch for which you want the paleo layer, e.g.
#'   \code{"mid-Holocene", "Last Glacial Maximum"}.
#' @param years_ago integer. Years for which you want the paleo layer, e.g.
#'   \code{6000, 21000}.
#'   
#' @details Stops with an exception if no matching paleo layer was found for one
#'   or more of the provided current climate layer codes.
#'   
#' @return A dataframe with information on the paleo layer(s) matching the
#'   provided current layer(s).
#'   
#' @examples
#' paleo_layers <- get_paleo_layers("MS_biogeo08_sss_mean_5m", years_ago = 6000)
#' paleo_layers$layer_code
#' @export
#' @seealso \code{\link{list_layers_paleo}}, \code{\link{list_layers}},
#'   \code{\link{load_layers}}
get_paleo_layers <- function(current_layer_codes, model_name = NA, epoch = NA, years_ago = NA) {
  if(is.data.frame(current_layer_codes)) {
    current_layer_codes <- current_layer_codes$layer_code
  }
  data <- get_sysdata()$layerlistpaleo
  m <- data$current_layer_code %in% current_layer_codes  & 
    (is.na(model_name) | data$model_name %in% model_name) & 
    (is.na(epoch) | data$epoch %in% epoch) & 
    (is.na(years_ago) | data$years_ago %in% years_ago)
  data <- data[m,]
  current_found <- current_layer_codes %in% data$current_layer_code
  if(!all(current_found)) {
    stop(paste0("paleo layer code not found for layercodes: ", 
                paste0(current_layer_codes[!current_found], collapse = ", "), 
                " and model name = ", model_name,
                " and epoch = ", epoch,
                " and years ago = ", years_ago))
  }
  if(NROW(data) > length(current_layer_codes)) {
    warning("More than one paleo layer found for one or more current layer codes")
  }
  return(data)
}

#' Layer info for specific layer codes
#' 
#' \code{get_layers_info} returns all detailed information on the current or
#' future climate layers of one or more datasets.
#' 
#' @usage get_layers_info(layer_codes)
#' @param layer_codes character vector. Vector with the layer codes of the
#'   layers you want the full information for. This can also be a dataframe with
#'   as column \code{layer_code}.
#' @return A list with four dataframes \code{common}, \code{current},
#'   \code{future} and \code{paleo}, the \code{common} dataframe contains data
#'   for all shared columns in the other three dataframes. The other dataframes
#'   contain all detailed information on the layer(s) matching the layer codes.
#'   
#' @examples 
#' info <- get_layers_info(c("BO_salinity", "BO_B1_2100_salinity"))
#' info$common
#' info$current
#' info$future
#' info$paleo
#' @export
#' @seealso \code{\link{list_layers}}, \code{\link{list_layers_future}}, 
#'   \code{\link{list_layers_paleo}}, \code{\link{load_layers}}
get_layers_info <- function(layer_codes) {
  if(is.data.frame(layer_codes)) {
    layer_codes <- layer_codes$layer_code
  }
  current <- get_sysdata()$layerlist
  future <- get_sysdata()$layerlistfuture
  paleo <- get_sysdata()$layerlistpaleo
  current <- current[current$layer_code %in% layer_codes,]
  future <- future[future$layer_code %in% layer_codes,]
  paleo <- paleo[paleo$layer_code %in% layer_codes,]
  common_cols <- c("dataset_code", "layer_code", "name", "description", "terrestrial", "marine", "cellsize_equalarea", "cellsize_lonlat", "units", "derivation", "month")
  common <- rbind(cbind(time=rep("current", NROW(current)), current[,common_cols]), 
                  cbind(time=rep("future", NROW(future)), future[,common_cols]), 
                  cbind(time=rep("paleo", NROW(paleo)), paleo[,common_cols]))
  list(common = common, current = current, future = future, paleo = paleo)
}