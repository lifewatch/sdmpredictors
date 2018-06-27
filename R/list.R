#' Lists the supported datasets
#'
#' \code{list_datasets} returns information on the supported datasets.
#'
#' @usage list_datasets(terrestrial = NA, marine = NA, freshwater = NA)
#'
#' @param terrestrial logical. When \code{TRUE}, then datasets that only have
#'   terrestrial data (seamasked) are returned.
#' @param marine logical. When \code{TRUE}, then datasets that only have marine
#'   data (landmasked) are returned.
#' @param freshwater logical. When \code{TRUE}, then datasets that only have
#'   freshwater data are returned.
#'
#' @details By default it returns all datasets, when both \code{marine}, \code{freshwater} and
#'   \code{terrestrial} are \code{FALSE} then only datasets without land- nor
#'   seamasks are returned.
#'
#' @return A dataframe with information on the supported datasets.
#'
#' @examples
#' list_datasets()
#' list_datasets(marine=TRUE)
#' list_datasets(terrestrial=TRUE)
#'
#' @export
#' @seealso \code{\link{list_layers}}, \code{\link{list_layers_future}},
#'   \code{\link{list_layers_paleo}}
list_datasets <- function(terrestrial = NA, marine = NA, freshwater = NA) {
  data <- get_sysdata()$datasetlist
  data <- filterbydatatype(data, terrestrial, marine, freshwater)
  return(data)  
}

#' List the current climate layers provided by one or more datasets
#'
#' \code{list_layers} returns information on the layers of one or more datasets.
#'
#' @usage list_layers(datasets=c(), terrestrial = NA, marine = NA, freshwater =
#'   NA, monthly = TRUE, version = NULL)
#'
#' @param datasets character vector. Code of the datasets.
#' @param terrestrial logical. When \code{TRUE} (default), then datasets that
#'   only have terrestrial data (seamasked) are returned.
#' @param marine logical. When \code{TRUE} (default), then datasets that only
#'   have marine data (landmasked) are returned.
#' @param freshwater logical. When \code{TRUE}, then datasets that only have
#'   freshwater data are returned.
#' @param monthly logical. When \code{FALSE}, then no monthly layers are
#'   returned. All annual and monthly layers are returned by default.
#' @param version numeric vector. When \code{NULL} then layers from all versions
#'   of datasets are returned (default) else layers are filtered by version
#'   number.
#'
#' @details By default it returns all layers from all datasets, when both marine
#'   and terrestrial are \code{FALSE} then only layers from datasets without
#'   land- nor seamasks are returned. Layers for paleo and future climatic
#'   conditions can be listed with \code{\link{list_layers_paleo}} and
#'   \code{\link{list_layers_future}}. Available paleo and future climate layers
#'   for a current climate layer code can be listed with the functions
#'   \code{\link{get_paleo_layers}} and \code{\link{get_future_layers}}.
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
#'   \code{\link{list_layers_future}}, \code{\link{list_layers_paleo}},
#'   \code{\link{get_future_layers}}, \code{\link{get_paleo_layers}}
list_layers <- function(datasets=c(), terrestrial = NA, marine = NA, freshwater = NA, monthly = TRUE, version = NULL) {
  data <- get_sysdata()$layerlist
  data <- filterbydatatype(data, terrestrial, marine, freshwater)
  
  if(is.data.frame(datasets)) {
    datasets <- datasets$dataset_code
  }
  if (length(datasets) > 0) {
    data <- data[data$dataset_code %in% datasets,]
  }
  if (!monthly) {
    data <- data[is.na(data$month),]
  }
  if(is.numeric(version) & length(version) > 0) {
    data <- data[data$version %in% version,]
  }
  return(data)
}

#' List the future climate layers provided by one or more datasets
#' 
#' \code{list_layers_future} returns information on the future climate layers of
#' one or more datasets.
#' 
#' @usage list_layers_future(datasets = c(), scenario = NA, year = NA,
#'   terrestrial = NA, marine = NA, freshwater = NA, version = NULL)
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
#' @param freshwater logical. When \code{TRUE}, then datasets that only have
#'   freshwater data are returned.
#' @param monthly logical. When \code{FALSE}, then no monthly layers are 
#'   returned. All annual and monthly layers are returned by default.
#' @param version numeric vector. When \code{NULL} then layers from all versions
#'   of datasets are returned (default) else layers are filtered by version
#'   number.
#'   
#' @details By default it returns all layers from all datasets, when both marine
#'   and terrestrial are \code{FALSE} then only layers without land- nor 
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
                               terrestrial = NA, marine = NA, freshwater = NA, monthly = TRUE, version = NULL) {
  data <- get_sysdata()$layerlistfuture
  
  if(!is.na(scenario)) {
    data <- data[data$scenario %in% scenario,]
  }
  if(!is.na(year)) {
    data <- data[data$year %in% year,]
  }
  data <- filter_layers(data, datasets, terrestrial, marine, freshwater, monthly, version)
  return(data)
}


#' Get the name of future climate layer(s) based on the current climate
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


#' List the paleo climate layers provided by one or more datasets
#' 
#' \code{list_layers_paleo} returns information on the paleo climate layers of 
#' one or more datasets.
#' 
#' @usage list_layers_paleo(datasets=c(), model_name=NA, epoch=NA, years_ago=NA,
#'   terrestrial=TRUE, marine=TRUE, monthly=TRUE, version=NULL)
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
#' @param freshwater logical. When \code{TRUE}, then datasets that only have
#'   freshwater data are returned.
#' @param monthly logical. When \code{FALSE}, then no monthly layers are 
#'   returned. All annual and monthly layers are returned by default.
#' @param version numeric vector. When \code{NULL} then layers from all versions
#'   of datasets are returned (default) else layers are filtered by version
#'   number.
#'   
#' @details By default it returns all layers from all datasets, when both marine
#'   and terrestrial are \code{FALSE} then only layers without land- nor 
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
                               terrestrial = NA, marine = NA, freshwater = NA, monthly = TRUE, version = NULL) {
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
  data <- filter_layers(data, datasets, terrestrial, marine, freshwater, monthly, version)
  return(data)
}

#' Get the name of paleo climate layer(s) based on the current climate
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
#' @usage get_layers_info(layer_codes = c())
#' @param layer_codes character vector. Vector with the layer codes of the 
#'   layers you want the full information for. This can also be a dataframe with
#'   as column \code{layer_code}.
#' @return A list with four dataframes \code{common}, \code{current}, 
#'   \code{future} and \code{paleo}, the \code{common} dataframe contains data 
#'   for all shared columns in the other three dataframes. The other dataframes 
#'   contain all detailed information on the layer(s) matching the layer codes.
#'   By default information for all layers is returned.
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
get_layers_info <- function(layer_codes = c()) {
  if(is.data.frame(layer_codes)) {
    layer_codes <- layer_codes$layer_code
  }
  current <- get_sysdata()$layerlist
  future <- get_sysdata()$layerlistfuture
  paleo <- get_sysdata()$layerlistpaleo
  if(!is.null(layer_codes)) {
    current <- current[na.omit(match(layer_codes, current$layer_code)),]
    future <- future[na.omit(match(layer_codes, future$layer_code)),]
    paleo <- paleo[na.omit(match(layer_codes, paleo$layer_code)),]
  }
  common_cols <- c("dataset_code", "layer_code")
  common <- rbind(cbind(time=rep("current", NROW(current)), current[,common_cols]), 
                  cbind(time=rep("future", NROW(future)), future[,common_cols]), 
                  cbind(time=rep("paleo", NROW(paleo)), paleo[,common_cols]))
  list(common = common, current = current, future = future, paleo = paleo)
}

filter_layers <- function(layers, datasets, terrestrial, marine, freshwater, monthly, version) {
  # filter terrestrial/marine and/or given datasets
  datasets_filter <- list_datasets(terrestrial, marine, freshwater)$dataset_code
  if(is.data.frame(datasets)) {
    datasets <- datasets$dataset_code
  }
  if (length(datasets) > 0) {
    datasets <- datasets[datasets %in% datasets_filter]
  } else {
    datasets <- datasets_filter
  }
  layers <- layers[layers$dataset_code %in% datasets,]
  # filter monthly
  if (!monthly) {
    current_layer_info <- get_layers_info(layers$current_layer_code)$current
    layers <- layers[is.na(current_layer_info$month),]
  }
  # filter version
  if(is.numeric(version) & length(version) > 0) {
    layers <- layers[layers$version %in% version,]
  }
  layers
}

filterbydatatype <- function(data, terrestrial, marine, freshwater) {
  if(isTRUE(marine) | isTRUE(freshwater)) {
    terrestrial <- isTRUE(terrestrial)
  } else if(isTRUE(terrestrial) | isTRUE(freshwater)) {
    marine <- isTRUE(marine)
  } else if(isTRUE(terrestrial) | isTRUE(marine)) {
    freshwater <- isTRUE(freshwater)
  }
  if(!is.na(terrestrial) && !terrestrial) {
    data <- subset(data, !terrestrial)
  }
  if(!is.na(marine) && !marine) {
    data <- subset(data, !marine)
  }
  if(!is.na(freshwater) && !freshwater) {
    data <- subset(data, !freshwater)
  }
  data
}