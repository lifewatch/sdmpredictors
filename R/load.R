#' Load layers
#' 
#' Method to load rasters from disk or from the internet.
#' By default a RasterStack is returned but this is only possible When all rasters have 
#' the same spatial extent and resolution.
#' 
#' @usage
#' load_layers(layercodes, datadir = ".", equalarea = TRUE, rasterstack = TRUE)
#' 
#' @param layercodes character vector or dataframe. Layer_codes of the 
#' layers to be loaded or dataframe with a "layer_code" column.
#' @param datadir character. Directory where you want to store the data, by default rasters 
#' are stored in the current directory.
#' @param equalarea logical. If \code{TRUE} then layers are loaded with a Behrmann 
#' cylindrical equal-area projection, otherwise unprojected (lonlat).
#' @param rasterstack logical. If \code{TRUE} (default value) then the result is a 
#' \code{\link[raster]{stack}} otherwise a list of rasters is returned.
#'
#' @return RasterStack or list of raster
#' 
#' @export
#' @seealso 
#' \code{\link{list_layers}}, \code{\link{layer_stats}}, \code{\link{layers_correlation}}
load_layers <- function(layercodes, datadir = ".", equalarea = TRUE, rasterstack = TRUE) {
  ## TODO replace datadir with the rasterOptions approach from the raster package ???
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