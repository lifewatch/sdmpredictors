#' Get data directory
#' 
#' Find out where the environmental data is stored
#' 
#' @usage get_datadir(datadir)
#'   
#' @param datadir character. Directory as passed to \code{\link{load_layers}}.
#'   This does not change the data directory used by \code{\link{load_layers}}
#'   but only shows the result of passing a specific datadir. If \code{NULL} is
#'   passed then the \code{sdmpredictors_datadir} option is read. To set this
#'   run \code{options(sdmpredictors_datadir = "<your preferred directory>")} in
#'   every session or in your .RProfile.
#'   
#' @return Path to the data directory.
#' @seealso \code{\link{load_layers}}
#' @keywords internal
get_datadir <- function(datadir) {
  if(is.null(datadir)) {
    datadir <- getOption("sdmpredictors_datadir")
    if(is.null(datadir)) {
      stop("No datadir provided and the \"sdmpredictors_datadir\" option is NULL, set options(sdmpredictors_datadir=\"<your directory>\") or set the datadir parameter in load_layers")
    }
  }
  if(!dir.exists(datadir)) {
    dir.create(datadir, recursive = TRUE)
  }
  normalizePath(paste0(datadir), winslash = "/", mustWork = TRUE)
}

#' Load layers
#' 
#' Method to load rasters from disk or from the internet. By default a 
#' RasterStack is returned but this is only possible When all rasters have the 
#' same spatial extent and resolution.
#' 
#' @usage load_layers(layercodes, equalarea=FALSE, rasterstack=TRUE, 
#'   datadir=NULL)
#'   
#' @param layercodes character vector or dataframe. Layer_codes of the layers to
#'   be loaded or dataframe with a "layer_code" column.
#' @param equalarea logical. If \code{TRUE} then layers are loaded with a 
#'   Behrmann cylindrical equal-area projection (\code{\link{equalareaproj}}), 
#'   otherwise unprojected (\code{\link{lonlatproj}}). Default is \code{FALSE}.
#' @param rasterstack logical. If \code{TRUE} (default value) then the result is
#'   a \code{\link[raster]{stack}} otherwise a list of rasters is returned.
#' @param datadir character. Directory where you want to store the data. If 
#'   \code{NULL} is passed (default) then the \code{sdmpredictors_datadir} 
#'   option is read. To set this run \code{options(sdmpredictors_datadir="<your 
#'   preferred directory>")} in every session or add it to your .RProfile.
#'   
#' @return RasterStack or list of raster
#' @examples \dontrun{
#' # warning using tempdir() implies that data will be downloaded again in the 
#' # next R session
#' env <- load_layers("BO_calcite", datadir = tempdir())
#' }
#' @export
#' @seealso \code{\link{list_layers}}, \code{\link{layer_stats}}, 
#'   \code{\link{layers_correlation}}
load_layers <- function(layercodes, equalarea = FALSE, rasterstack = TRUE, datadir = NULL) {
  if(is.na(equalarea) || !is.logical(equalarea) && length(equalarea) != 1) {
    stop("equalarea should be TRUE or FALSE")
  }
  if(is.data.frame(layercodes)) {
    layercodes <- layercodes$layer_code
  }
  info <- get_layers_info(layercodes)
  counts <- table(info$common$time)
  if(length(unique(info$common$cellsize_equalarea)) > 1) {
    stop("Loading layers with different cellsize is not supported")
  } else if (sum(counts) != NROW(layercodes)) {
    layers <- info$common$layer_code
    stop(paste0("Following layer codes where not recognized: ", 
                paste0(layercodes[!(layercodes %in% layers)], collapse = ", ")))
  }
  if(max(counts) != NROW(layercodes)) {
    warning("Layers from different eras (current, future, paleo) are being loaded together")
  }
  datadir <- get_datadir(datadir)
  urlroot <- get_sysdata()$urldata
  get_layerpath <- function(layercode) {
    suffix <- ifelse(equalarea, "", "_lonlat")
    path <- paste0(datadir, "/", layercode, suffix, ".tif")
    if(!file.exists(path)) {
      urlroot <- "http://www.phycology.ugent.be/research/sdmpredictors/"
      url <- paste0(urlroot, layercode, suffix, ".tif")
      download.file(url, path, method = "auto", quiet = FALSE, mode = "wb")
    }
    ifelse(file.exists(path), path, NA)
  }
  
  paths <- sapply(layercodes, get_layerpath)
  if(rasterstack) {
    st <- raster::stack(paths)
    names(st) <- sub("_lonlat$", "", names(st))
    return(st)
  } else {
    return(lapply(paths, function(path) { 
      r <- raster::raster(path) 
      names(r) <- sub("_lonlat$", "", names(r))
      r}))
  }
}

#' Longitude/latitude coordinate reference system (EPSG:4326), used when using
#' load_layers with equal_area = FALSE
#' @export
lonlatproj <- sp::CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

#' World Behrmann equal area coordinate reference system (ESRI:54017), used when
#' using load_layers with equal_area = TRUE
#' @export
equalareaproj <- sp::CRS("+proj=cea +lon_0=0 +lat_ts=30 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0")