#' Gives basic layer statistics
#' 
#' \code{layer_stats} returns basic statistics (minimum, q1, median, q3,
#' maximum, median absolute deviation (mad), mean, standard deviation (sd))
#' about each given layercode.
#' 
#' @usage layer_stats(layercodes = c())
#'   
#' @param layercodes character vector or dataframe. Codes of the layers you want
#'   the basic statistics of as a character vector or a dataframe with a
#'   "layer_code" column. With the default empty vector all statistics are
#'   returned.
#'   
#' @return A dataframe with basic statistics about each given layercode.
#'   
#' @examples
#' # layer stats for the first 10 layers
#' layer_stats()[1:10,]
#' layer_stats(c("BO_calcite", "MS_bathy_5m"))
#' 
#' @export
#' @seealso \code{\link{list_layers}} \code{\link{layers_correlation}}
layer_stats <- function(layercodes = c()) {
  d <- get_sysdata()$layerstats
  if(is.data.frame(layercodes)) {
    layercodes <- layercodes$layer_code
  }
  if(!is.null(layercodes)) {
    notfound <- layercodes[!(layercodes %in% d$layer_code)]
    d <- d[d$layer_code %in% layercodes,]
    if(length(notfound) > 0) {
      warning(paste0("Following layercode(s) where not found: ['", 
                     paste(notfound, collapse = "', '"), "']"))
    }
  }
  return(d)
}

#' Gives the Pearson correlation between layers
#' 
#' \code{layers_correlations} returns the Pearson product-moment correlation
#' coefficient (Pearson's r) for every combination of the give layercodes. The
#' correlation between a terrestrial and a marine layer has been set to
#' \code{NA}.
#' 
#' @usage layers_correlation(layercodes = c(), include_quadratic = TRUE)
#'   
#' @param layercodes character vector or dataframe. Codes of the layers you want
#'   the basic statistics of as a character vector or a dataframe with a
#'   "layer_code" column. With the default empty vector all statistics are
#'   returned.
#' @param include_quadratic logical. When \code{TRUE}, then the correlation
#'   coefficients of the square of the layercodes area also returned. These
#'   layers are indicated with the layercode with \code{"_quadratic"} as a
#'   suffix, for example \code{"BO_calcite"} became 
#'   \code{"BO_calcite_quadratic"}.
#'   
#' @return A dataframe with the Pearson product-moment correlation coefficients.
#'   
#' @examples
#' # correlation of the first 10 layers
#' layers_correlation()[1:10,1:10]
#' layers_correlation(c("BO_calcite", "MS_bathy_5m"))
#' layers_correlation(c("BO_calcite", "MS_bathy_5m"), include_quadratic = FALSE)
#' 
#' @export
#' @seealso \code{\link{list_layers}} \code{\link{layer_stats}}
#'   \code{\link{correlation_groups}}
#' @encoding UTF-8
layers_correlation <- function(layercodes = c(), include_quadratic = TRUE) {
  d <- get_sysdata()$layerscorrelation
  if(is.data.frame(layercodes)) {
    layercodes <- layercodes$layer_code
  }
  if(!is.null(layercodes)) {
    if(include_quadratic) {
      layercodes <- c(layercodes, paste0(layercodes, "_quadratic"))
    }
    notfound <- layercodes[!(layercodes %in% colnames(d))]
    d <- d[rownames(d) %in% layercodes,colnames(d) %in% layercodes, drop = FALSE]
    if(length(notfound) > 0) {
      warning(paste0("Following layercode(s) where not found: ['", 
                     paste(notfound, collapse = "', '"), "']"))
    }
  }
  else if(!include_quadratic) {
    filter <- !grepl("_quadratic$", rownames(d))
    d <- d[filter, filter, drop = FALSE]
  }
  return(d)
}


#' Groups layers based on the Pearson correlation
#' 
#' \code{correlation_groups} returns groups of layer codes such as each layer
#' from one group has an absolute Pearson product-moment correlation coefficient
#' (Pearson's r) that is smaller than the maximum_correlation (default 0.7) with
#' each variable in any other group. The correlation values of quadratic layers
#' are used for creating the groups but only non quadratic layer codes are 
#' returned.
#' 
#' @usage correlation_groups(layers_correlation, max_correlation=0.7)
#'   
#' @param layers_correlation matrix or dataframe. A square matrix with the
#'   layers correlations  you want to group.
#' @param max_correlation number. The maximum correlation 2 layers may have
#'   before they are put in the same correlation group.
#'   
#' @return A list of vectors with each vector containing the layer codes of one
#'   correlation group.
#'   
#' @references Dormann, C. F., Elith, J., Bacher, S., Buchmann, C., Carl, G.,
#'   Carre, G., ... Lautenbach, S. (2013). Collinearity: a review of methods to
#'   deal with it and a simulation study evaluating their performance.
#'   Ecography, 36(1), 027-046. doi:10.1111/j.1600-0587.2012.07348.x 
#'   Barbet-Massin, M. & Jetz, W. (2014). A 40-year, continent-wide,
#'   multispecies assessment of relevant climate predictors for species
#'   distribution modelling. Diversity and Distributions, 20(11), 1285-1295.
#'   doi:10.1111/ddi.12229
#'   
#' @examples
#' 
#' corr <- layers_correlation(c("BO_calcite", "BO_damin", "MS_bathy_5m"))
#' print(corr)
#' print(correlation_groups(corr, max_correlation=0.6))
#' 
#' @export
#' @seealso \code{ \link{layers_correlation} \link{list_layers}
#'   \link{layer_stats}}
#' @encoding UTF-8
correlation_groups <- function(layers_correlation, max_correlation = 0.7) {
  to_set <- function(i) { 
    ## returns only non quadratic layer codes BUT makes sure to make use of the quadratic correlation value
    row <- layers_correlation[i,,drop = FALSE]
    layer_codes <- names(row)[abs(row) > max_correlation]
    unique(sub("_quadratic$","", layer_codes))
  }
  group_count <- function (t) { sum(sapply(t, function(s) { ifelse(length(s) > 0, 1, 0) })) }
  t <- lapply(1:nrow(layers_correlation), to_set)
  
  if(nrow(layers_correlation) > 1) {
    previous_group_count <- group_count(t)
    new_group_count <- -1
    while(previous_group_count != new_group_count){
      for (i in 1:(length(t)-1)){
        s1 <- t[[i]]
        for (j in (i+1):length(t)){
          s2 <- t[[j]]
          if(length(intersect(s1,s2)) > 0) {
            t[[i]] <- union(s1,s2)
            t[[j]] <- character() # empty non null vector, note that c() is NULL
          }
        }
      }
      previous_group_count <- new_group_count
      new_group_count <- group_count(t)
    }
  }
  t <- Filter(function(s) { length(s) > 0}, t) # filter empty groups
  t <- lapply(t, function(s) { sapply(s, identity) }) # convert to vector of layer codes
  return(t)
}


#' Calculate statistics for a given raster.
#' 
#' Internal method used to calculate the statistics of the layers.
#' 
#' @usage calc_stats(layercode, raster)
#' 
#' @param layercode character.
#' @param raster RasterLayer.
#'   
#' @return A dataframe with the layercode and all basic statistics.
#'   
#' @seealso \code{\link{layer_stats}}
#' @keywords internal
calc_stats <- function(layercode, raster) {
  v <- raster::values(raster)
  q <- quantile(v,na.rm=TRUE)
  d <- data.frame(layer_code = layercode,
                  minimum = q[1],
                  q1 = q[2],
                  median = q[3],
                  q3 = q[4],
                  maximum = q[5],
                  mad = mad(v, center = q[3], na.rm = TRUE),
                  mean = mean(v, na.rm = TRUE),
                  sd = sd(v, na.rm = TRUE),
                  moran = raster::Moran(raster),
                  geary = raster::Geary(raster),
                  stringsAsFactors = FALSE)
  return (d)
}
