#' Generate dataset citations
#' 
#' \code{dataset_citations} returns dataset citations as text or as 
#' "\code{\link[utils]{bibentry}}" objects.
#' 
#' @usage dataset_citations(datasets = c(), astext = TRUE)
#'   
#' @param datasets character vector. Code of the datasets. When no datasets are 
#'   provided (default), then all citations are returned.
#' @param astext logical. When \code{TRUE} (default), then citations are 
#'   returned as text otherwise they are returned as 
#'   "\code{\link[utils]{bibentry}}" objects.
#'   
#' @details Note that in order to generate a full list of citations it is
#'   preferable to run the \code{\link{layer_citations}} function.
#'   
#' @return Either a character vector or a list of 
#'   "\code{\link[utils]{bibentry}}" objects.
#'   
#' @examples
#' # print the Bio-ORACLE citation
#' print(dataset_citations("Bio-ORACLE"))
#' 
#' # print all citations as Bibtex
#' print(lapply(dataset_citations(astext = FALSE), toBibtex))
#' @export
#' @seealso \code{\link{layer_citations}}, \code{\link[utils]{bibentry}}, 
#'   \code{\link{list_datasets}}
dataset_citations <- function(datasets = c(), astext = TRUE) {
  bibentries <- get_sysdata()$bibentries
  
  if(is.data.frame(datasets)) {
    datasets <- datasets$dataset_code
  } else if (is.null(datasets)) {
    datasets <- setdiff(names(bibentries), "lnk_bibentry")
  }
  if(astext) {
    as.vector(sub("\\n", " ", sapply(bibentries[datasets], format)))
  } else {
    bibentries[datasets]
  }
}

#' Generate citations for all layers
#' 
#' \code{layer_citations} returns layer citations as text or as 
#' "\code{\link[utils]{bibentry}}" objects.
#' 
#' @usage layer_citations(layers = c(), astext = TRUE)
#'   
#' @param layers character vector. Code of the layers from past, current and future climate layers. When no layers are
#'   provided (default), then all citations are returned.
#' @param astext logical. When \code{TRUE} (default), then citations are 
#'   returned as text otherwise they are returned as 
#'   "\code{\link[utils]{bibentry}}" objects.
#'   
#' @details Note that for some layers multiple references are returned as some
#'   of the predictors have been published separately.
#'   
#' @return Either a character vector or a list of 
#'   "\code{\link[utils]{bibentry}}" objects.
#'   
#' @examples
#' # print the citation for the Bio-ORACLE salinity layer
#' print(layer_citations("BO_salinity"))
#' 
#' # print the citation for a MARSPEC paleo layer
#' print(layer_citations("MS_biogeo02_aspect_NS_21kya"))
#' 
#' # print all citations as Bibtex
#' print(lapply(layer_citations(astext = FALSE), toBibtex))
#' @export
#' @seealso \code{\link{layer_citations}}, \code{\link[utils]{bibentry}}, \code{\link{list_datasets}}
layer_citations <- function(layers = c(), astext = TRUE) {
  sysdata <- get_sysdata()
  bibentries <- sysdata$bibentries
  
  if(is.data.frame(layers)) {
    layers <- layers$layer_code
  }
  if (is.null(layers)) {
    layers <- get_layers_info()$common$layer_code
  }
  current <- sysdata$layerlist[sysdata$layerlist$layer_code %in% layers,]
  future <- sysdata$layerlistfuture[sysdata$layerlistfuture$layer_code %in% layers,]
  paleo <- sysdata$layerlistpaleo[sysdata$layerlistpaleo$layer_code %in% layers,]
  
  bibkeys <- unique(c(current$dataset_code, 
                      future$dataset_code, 
                      paleo$dataset_code,
                      unlist(bibentries$lnk_bibentry$future[unique(future$dataset_code)]),
                      unlist(bibentries$lnk_bibentry$paleo[unique(paleo$dataset_code)]),
                      unlist(bibentries$lnk_bibentry$layers[layers])))
  
  entries <- bibentries[bibkeys]
  
  if(astext) {
    as.vector(sub("\\n", " ", sapply(entries, format)))
  } else {
    entries
  }
}