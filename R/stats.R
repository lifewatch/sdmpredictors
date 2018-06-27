#' Gives basic layer statistics
#' 
#' \code{layer_stats} returns basic statistics (minimum, q1, median, q3,
#' maximum, median absolute deviation (mad), mean, standard deviation (sd))
#' for each given layercode.
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
#' @usage layers_correlation(layercodes = c())
#'   
#' @param layercodes character vector or dataframe. Codes of the layers, you
#'   want the correlation matrix of, as a character vector or a dataframe with a
#'   "layer_code" column. With the default empty vector the correlation between
#'   all layers is returned.
#'   
#' @return A dataframe with the Pearson product-moment correlation coefficients.
#'   
#' @examples
#' # correlation of the first 10 layers
#' layers_correlation()[1:10,1:10]
#' layers_correlation(c("BO_calcite", "MS_bathy_5m"))
#' layers_correlation(c("BO_calcite", "MS_bathy_5m"))
#' 
#' @export
#' @seealso \code{\link{list_layers} \link{layer_stats} 
#'   \link{correlation_groups} \link{plot_correlation}}
#' @encoding UTF-8
layers_correlation <- function(layercodes = c()) {
  d <- get_sysdata()$layerscorrelation
  if(is.data.frame(layercodes)) {
    layercodes <- layercodes$layer_code
  }
  if(!is.null(layercodes)) {
    found <- layercodes[(layercodes %in% colnames(d))]
    notfound <- layercodes[!(layercodes %in% colnames(d))]
    d <- d[found, found, drop = FALSE]
    if(length(notfound) > 0) {
      warning(paste0("Following layercode(s) where not found: ['", 
                     paste(notfound, collapse = "', '"), "']"))
    }
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
#'   layers correlations you want to group.
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
  layers_correlation <- as.data.frame(layers_correlation)
  to_set <- function(i) { 
    row <- layers_correlation[i,,drop = FALSE]
    layer_codes <- names(row)[abs(row) > max_correlation]
    layer_codes
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

#' Plot the correlation matrix for the provided layercodes
#' 
#' #' \code{plot_correlation} creates a plot of the correlation between 
#' different layers
#' 
#' @usage plot_correlation(layers_correlation, prettynames = list(),
#'   palette = c("#2c7bb6", "#abd9e9", "#ffffbf", "#fdae61", "#d7191c"))
#'   
#' @param layers_correlation matrix or dataframe. A square matrix with the
#'   layers correlations you want to plot as returned by 
#'   \code{\link{layers_correlation}} or 
#'   \code{\link{pearson_correlation_matrix}}.
#' @param prettynames list. Optional list with as names the layercodes and as 
#'   values the name of the layer to be used in the plot.
#' @param palette character vector. optional vector with 5 entries for the range
#'   of colors to be used for the correlation matrix plot.
#'   
#' @return A ggplot object that can be printed or saved.
#'   
#' @details This function requires ggplot2 and plots the correlations for the 
#'   layers in the same order as the layercodes are provided to this function.
#'   
#' @examples
#' correlation <- layers_correlation(c("BO_calcite", "BO_damin", "MS_bathy_5m"))
#' p <- plot_correlation(correlation)
#' print(p)
#' 
#' @export
#' @seealso \code{ \link{layers_correlation} \link{pearson_correlation_matrix} 
#'   \link{list_layers} \link{layer_stats} \link{correlation_groups}}
#' @encoding UTF-8
plot_correlation <- function(layers_correlation, prettynames = list(), palette = c("#2c7bb6", "#abd9e9", "#ffffbf", "#fdae61", "#d7191c")) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("ggplot2 needed for the 'plot_correlation' function to work. Please install it.",
         call. = FALSE)
  }
  if (!requireNamespace("reshape2", quietly = TRUE)) {
    stop("reshape2 needed for the 'plot_correlation' function to work. Please install it.",
         call. = FALSE)
  }
  stopifnot(length(palette) == 5)
  stopifnot(colnames(cm) == rownames(cm))
  
  cm <- as.data.frame(layers_correlation)
  layercodes <- colnames(cm)
  not_found <- layercodes[sapply(prettynames[layercodes], is.null)]
  prettynames[not_found] <- not_found
  
  m <- reshape2::melt(as.matrix(cm))
  m$Var1 <- factor(as.character(m$Var1), levels=layercodes, labels=unlist(prettynames[layercodes]))
  m$Var2 <- factor(as.character(m$Var2), levels=layercodes, labels=unlist(prettynames[layercodes]))
  breaks <- c(-1.06, -0.7, -0.5, 0, 0.5, 0.7, 1.06)
  
  p <- ggplot2::ggplot(data = m, ggplot2::aes_string(x="Var1", y="Var2", fill="value")) + 
    ggplot2::geom_tile() +
    ggplot2::scale_fill_gradientn(colors=palette, breaks=breaks, limits=c(-1.06, 1.06), labels=c(-1,"",-0.5,0,0.5,"",1)) +
    ggplot2::labs(x= "", y="", fill = "Correlation")  +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, hjust = 1, vjust = 0, color = "black"),
                   axis.text.y = ggplot2::element_text(color = "black"),  
                   axis.ticks.length = ggplot2::unit(0, units="cm"),
                   panel.background = ggplot2::element_rect(fill = "white"),  
                   panel.border = ggplot2::element_blank(),
                   panel.grid.major = ggplot2::element_blank(),
                   panel.grid.minor = ggplot2::element_blank(),
                   legend.background = ggplot2::element_rect(fill = "white"),
                   legend.text = ggplot2::element_text(color = "black"),
                   plot.background = ggplot2::element_rect(color = "white", fill = "white"))
  return(p)
}

#' Calculate the Pearson correlation coefficient matrix for a rasterstack
#'
#' @usage pearson_correlation_matrix(x, cachesize = 20, same_mask = FALSE)
#'
#' @param x RasterStack. The stack of rasters you want to calculate the Pearson
#'   correlation coefficient matrix for. This can be obtained by calling
#'   \code{\link{load_layers}}.
#' @param cachesize integer. For how many rasters should the values be kept in
#'   local memory. By default this is set to 20, a parameter which works
#'   reasonably well on a windows computer with 8GB RAM.
#' @param same_mask logical. Whether we can assume that the mask is the same
#'   for all layers (same NA values), default is \code{FALSE}.
#' @return A correlation matrix.
#'
#' @examples \dontrun{
#' # calculate correlation between SST and salinity in the Baltic Sea
#'
#' # warning using tempdir() implies that data will be downloaded again in the
#' # next R session
#' x <- load_layers(c("BO_sstmax", "BO_salinity"), datadir = tempdir())
#' e <- extent(13, 31, 52, 66)
#' baltics <- crop(x, e)
#' print(pearson_correlation_matrix(baltics))
#' }
#' @export
#' @seealso \code{\link{layers_correlation} \link{plot_correlation}
#'   \link{load_layers}}
#' @encoding UTF-8
pearson_correlation_matrix <- function(x, cachesize = 20, same_mask = FALSE) {
  clear_raster_values_cache()
  on.exit(clear_raster_values_cache())
  asSample <- TRUE
  nl <- raster::nlayers(x)
  n <- raster::ncell(x)
  mat <- matrix(NA, nrow=nl, ncol=nl)
  colnames(mat) <- rownames(mat) <- names(x)
  
  sds <- c()
  mask <- NULL
  for(i in 1:nl) {
    if(same_mask && i == 1) {
      mask <- is.na(raster::values(raster(x, layer=1)))
    }
    sds[i] <- preprocess_raster_values(x, i, mask)
  }
  for (i in 1:nl) {
    mat[i,i] <- 1
  }
  for (i in seq(from = 0, to = nl-2, by = cachesize)) {
    indexes <- i + 1:cachesize
    v <- lapply(X = indexes[indexes <= nl], function(i) { raster_values(x, layer=i) })
    
    for( vi in 1:(length(v)-1)) { ## calculate correlations for all "cached" rasters
      for (vj in (vi+1):length(v)) {
        mat <- pearson_correlation(mat, indexes[vi], indexes[vj], v[[vi]], v[[vj]], sds, n, asSample)
      }
    }
    
    if(max(indexes) < nl) { ## calculate correlations for the other rasters
      for (j in (max(indexes)+1):nl) {
        jR <- raster_values(x, layer=j)
        for(vi in 1:length(v)) {
          mat <- pearson_correlation(mat, indexes[vi], j, v[[vi]], jR, sds, n, asSample)
        }
      }
    }
  }
  return(mat)
}

preprocess_raster_values <- function(x, layer, mask) {
  p <- file.path(get_datadir(NULL), paste0("cache_rv_", layer, ".rds"))
  v <- raster::values(raster(x, layer=layer))
  if(!is.null(mask)) {
    vna <- is.na(v)
    stopifnot(all(mask == vna)) # NA values are not the same as the mask
    v <- v[!vna]
  }
  m <- mean(v, na.rm=TRUE)
  v <- v - m
  print(paste0("Temporary cache file: ", p))
  saveRDS(v, p)
  sd(v, na.rm=TRUE)
}
raster_values <- function(x, layer) {
  p <- file.path(get_datadir(NULL), paste0("cache_rv_", layer, ".rds"))
  v <- readRDS(p)
}
clear_raster_values_cache <- function() {
  sapply(list.files(get_datadir(NULL), "^cache_rv_", full.names=TRUE), file.remove)
}

#' Calculate statistics for a given raster.
#' 
#' Method used to calculate the statistics of all layers. It can be re-used to
#' calculate statistics for a cropped version of the rasters.
#' 
#' @usage calculate_statistics(layercode, raster)
#'   
#' @param layercode character. Name of the layer.
#' @param raster RasterLayer. The raster you want to calculate statistics for.
#'   
#' @return A dataframe with the layercode and all basic statistics.
#' 
#' @examples \dontrun{
#' # calculate statistics of the SST and salinity in the Baltic Sea
#' 
#' # warning using tempdir() implies that data will be downloaded again in the 
#' # next R session
#' x <- load_layers(c("BO_sstmax", "BO_salinity"), datadir = tempdir())
#' e <- extent(13, 31, 52, 66)
#' baltics <- crop(x, e)
#' View(rbind(calculate_statistics("SST Baltic Sea", raster(x, layer = 1)))
#'            calculate_statistics("Salinity Baltic Sea", raster(x, layer = 2)))
#' }
#' @export
#' @seealso \code{\link{layer_stats}}
calculate_statistics <- function(layercode, raster) {
  v <- raster::values(raster)
  nav <- is.na(v)
  v <- v[!nav]
  q <- quantile(v)
  mean <- mean(v)
  mg <- moran_geary(raster, mean)
  d <- data.frame(layer_code = layercode,
                  minimum = q[1],
                  q1 = q[2],
                  median = q[3],
                  q3 = q[4],
                  maximum = q[5],
                  mad = mad(v, center = q[3]),
                  mean = mean,
                  sd = sd(v),
                  moran = mg$Moran,
                  geary = mg$Geary,
                  stringsAsFactors = FALSE)
  raster::removeTmpFiles(h=0)
  return (d)
}

moran_geary <- function(raster, mean) {
  rows <- 1:nrow(raster)
  row_chunks <- split(rows, ceiling(seq_along(rows)/min(nrow(raster),1000)))
  
  Eij <- 0
  wZiZj <- 0
  z2 <- 0
  n <- 0
  W <- 0
  gz <- 0
  for(chunk in row_chunks) {
    rowfocals <- getValuesFocal(raster, row=min(chunk), nrows=length(chunk), ngb=3, names=FALSE)
    center <- rowfocals[,5]
    z <- center - mean
    rowfocals[,5] <- NA
    wZiZj <- wZiZj + sum(rowSums(rowfocals - mean, na.rm = TRUE) * z, na.rm = TRUE)
    z2 <- z2 + sum(z * z, na.rm = TRUE)
    n <- n + sum(!is.na(z))
    W <- W + sum(!is.na(rowfocals))
    Eij <- Eij + sum((rowfocals - center) ^ 2, na.rm=TRUE)
    gz <- gz + sum(z ^ 2, na.rm=TRUE)
  }
  # Moran's I
  NS0 <- n / W
  mI <- NS0 * wZiZj/z2
  # Geary's C
  gz <- 2 * W * gz
  gC <- ((n - 1) * Eij/gz)
  list(Moran=mI, Geary=gC)
}


#' Calculate Pearson correlation coefficient for part of a matrix
#' @seealso \code{\link{layers_correlation} \link{pearson_correlation_matrix}}
#' @keywords internal
pearson_correlation <- function(mat, i, j, iR, jR, sds, n, asSample) {
  if (i <= nrow(mat) && j <= ncol(mat)) {
    if (i == j) {
      mat[i,j] = 1
    } else if (is.na(mat[i,j])) {
      r <- (iR * jR)
      v <- sum(r, na.rm=TRUE) / ((n - sum(is.na(r)) - asSample) * sds[i] * sds[j])
      mat[j,i] <- mat[i,j] <- v
    } else { 
      print(paste0("mat[i=", i, ",j=", j, "] is not NA (",mat[i,j],")")) 
    }
  }
  mat
}