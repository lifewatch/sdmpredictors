#' Load layers
#' 
#' Method to load rasters from disk or from the internet.
#' By default a RasterStack is returned but this is only possible When all rasters have 
#' the same spatial extent and resolution.
#' 
#' @usage
#' load_layers(layercodes, equalarea = TRUE, standardized = FALSE, rasterstack = TRUE, datadir = NULL)
#' 
#' @param layercodes character vector or dataframe. Layer_codes of the 
#' layers to be loaded or dataframe with a "layer_code" column.
#' @param equalarea logical. If \code{TRUE} then layers are loaded with a Behrmann 
#' cylindrical equal-area projection (\code{equalareaproj}) , otherwise unprojected (\code{lonlatproj}).
#' @param standardized logical. If \code{TRUE} then standardized rasters 
#' are returned ((x - mean) / sd). Default value is \code{FALSE}.
#' @param rasterstack logical. If \code{TRUE} (default value) then the result is a 
#' \code{\link[raster]{stack}} otherwise a list of rasters is returned.
#' @param datadir character. Directory where you want to store the data. The default value used is "." and 
#' can be overridden with \code{options(sdmpredictors_datadir = "<your preferred directory>")}.
#'
#' @return RasterStack or list of raster
#' 
#' @export
#' @seealso 
#' \code{\link{list_layers}}, \code{\link{layer_stats}}, \code{\link{layers_correlation}}
load_layers <- function(layercodes, equalarea = TRUE, standardized = FALSE, rasterstack = TRUE, datadir = NULL) {
  if(is.null(datadir)) {
    datadir <- getOption("sdmpredictors_datadir")
    if(is.null(datadir)) {
      datadir <- "."
    }
  }
  datadir <- normalizePath(paste0(datadir,"/"), winslash = "/", mustWork = TRUE)
  
  get_layerpath <- function(layercode) {
    get_layerpath_from_extension <- function(extension, suffix) {
      path <- paste0(datadir, layercode, suffix, extension)
      if (!file.exists(path)) {
        urlroot <- "http://www.phycology.ugent.be/research/sdmpredictors/"
        url <- paste0(urlroot, layercode, suffix, extension, ".gz")
        gzpath <- paste0(path, ".gz")
        download.file(url, gzpath, method = "auto", quiet = FALSE, mode = "wb")
        path <- decompress_file(gzpath, datadir)
      }
      return(path)
    }
    suffix <- ifelse(equalarea, "", "_lonlat")
    suffix <- paste0(suffix, ifelse(standardized, "_z", ""))
    grd <- get_layerpath_from_extension(".grd", suffix)
    gri <- get_layerpath_from_extension(".gri", suffix)
    if(file.exists(grd) & file.exists(gri)) {
      return(grd)
    } else {
      return(NA)
    }
  }
  if(is.data.frame(layercodes)) {
    layercodes <- layercodes$layer_code
  }
  if(rasterstack) {
    return(raster::stack(sapply(layercodes, get_layerpath)))
  } else {
    return(lapply(layercodes, function(lc) { raster::raster(load(lc)) }))
  }
}

#' Longitude/latitude coordinate reference system (EPSG:4326),
#' used when using load_layers with equal_area = FALSE
#' @export
lonlatproj <- sp::CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

#' World Behrmann equal area coordinate reference system (ESRI:54017), 
#' used when using load_layers with equal_area = TRUE
#' @export
equalareaproj <- sp::CRS("+proj=cea +lon_0=0 +lat_ts=30 +x_0=0 +y_0=0 +datum=WGS84 +ellps=WGS84 +units=m +no_defs")