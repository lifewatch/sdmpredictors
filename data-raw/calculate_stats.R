library(compiler)
enableJIT(3)


library(sdmpredictors)
library(raster)
library(dismo)
library(sp)
library(gplots)

statsdir <- "data-raw/stats"

# TODO define WorldClim layer codes, copy compressed to phycology site, then calc all terrestrial layer stats and all terrestrial correlations

calc_all_layer_stats <- function(terrestrial = FALSE, marine = FALSE, freshwater = FALSE) {
  calc_layer_stats <- function(layercode) {
    ## convert to behrmann first OR store data as Behrmann
    r <- load_layers(layercode, equalarea = TRUE)
    d <- sdmpredictors:::calculate_statistics(layercode, raster(r,1))
    return (d)
  }
  all_layercodes <- sdmpredictors::get_layers_info()$common$layer_code
  for (layercode in all_layercodes) { #sdmpredictors::list_layers(terrestrial = terrestrial, marine = marine)[,"layer_code"]) {
    fname <- paste0(statsdir, "/", layercode, ".rds")
    if(layercode != "WC_TODO" & !file.exists(fname)) {
      print(layercode)
      stats <- calc_layer_stats(layercode) ## TODO update calc_layer_stats so that it takes a raster or a layercode
      saveRDS(stats, fname)
    }
  }
}

new_layer_stats <- function(dir, layercode, overwrite = TRUE) {
  fname <- paste0(statsdir, "/", layercode, ".rds")
  if(overwrite || !file.exists(fname)) {
    r <- raster(file.path(dir, paste0(layercode, ".tif")))
    stats <- sdmpredictors:::calculate_statistics(layercode, r)
    saveRDS(stats, fname)
  }
}
# new_layer_stats("D:/a/projects/predictors/derived", "BO_shoredistance")


stats_biooracle2 <- function(overwrite) {
  layerpaths <- list.files("../../derived/biooracle2", "[.]tif$", full.names = TRUE)
  for(p in layerpaths) {
    if(!grepl("_lonlat.tif", basename(p))) {
      layercode <- sub("[.]tif$", "", basename(p))
      print(layercode)
      new_layer_stats("../../derived/biooracle2", layercode, overwrite)
    }
  }
}
# stats_biooracle2(overwrite = FALSE)

stats_envirem <- function() {
  layerpaths <- list.files("D:/a/data/ENVIREM", "[.]tif$", full.names = TRUE)
  for(layerpath in layerpaths) {
    p <- sub("[.]tif$", "", basename(layerpath))
    parts <- unlist(strsplit(p, "_5arcmin_"))
    if(parts[1] == "current") {
      layercode <- paste0("ER_",parts[2])
    } else {
      layercode <- paste0("ER_",parts[2],"_",parts[1])
    }
    print(layercode)
    new_layer_stats("D:/a/projects/predictors/derived/envirem", layercode)  
  }
}
# stats_envirem()

stats_worldclim <- function(overwrite) {
  layerpaths <- list.files("../../derived/worldclim_paleo_future", "[.]tif$", full.names = TRUE)
  for(p in layerpaths) {
    if(!grepl("_lonlat.tif", basename(p))) {
      layercode <- sub("[.]tif$", "", basename(p))
      print(layercode)
      new_layer_stats("../../derived/worldclim_paleo_future", layercode, overwrite)  
    }
  }
}
# stats_worldclim(overwrite = FALSE)

#calc_all_layer_stats(terrestrial = FALSE, marine = TRUE)
#calc_all_layer_stats(terrestrial = TRUE, marine = FALSE)

# stats and correlations
#calc_all_layer_stats(terrestrial = T); calc_all_correlation_matrices(terrestrial=T) 

get_all_layer_stats <- function(calc=FALSE) {
  if(calc) {
    calc_all_layer_stats()
  }
  d <- c()
  for (file in list.files(statsdir, pattern = "[.]rds", full.names = TRUE)) {
    d <- rbind(d, readRDS(file))
  }
  rownames(d) <- seq_len(nrow(d))
  return(d)
}

spatial_autocorrelation_range <- function(r) {
  ## r <- load_layers("BO_sstmin", datadir = rasterdir, equalarea=F)
  # input raster
  
#   library(usdm)
#   v <- Variogram(r, size=5) ## extremely slow
  
  sampleSize <- 100
  sample <- sampleRandom(r, size=sampleSize, na.rm=TRUE, xy=TRUE)
  
#   library(geoR)
#   v <- variog(coords = coordinates(sample[,1:2]), 
#               data = sample[,3])
#   plot(v)
  
  ## TODO convert to Behrmann for all stats OR 
  ## even before and only make behrmann rasters available ???
  
  
  ## VARIOGRAM WITH LAT LON DATA !!!!!
  library(gstat)
  point_data <- SpatialPointsDataFrame(sample[,1:2], as.data.frame(sample[,3]), proj4string=r@crs)
  gstat_variogram <- variogram(sample[, 3] ~ 1, data = point_data, alpha=c(0,45,90,135)) # alpha=N=0, NE=45, E=90, NW=135
  plot(gstat_variogram)
  gstat_variogram <- variogram(sample[, 3] ~ 1, data = point_data, cutoff=10000, width=100)
  plot(gstat_variogram)
  gstat_variogram_model <- fit.variogram(gstat_variogram, vgm(1, "Exp", 300, 1), fit.method=1)
  gstat_variogram_model <- fit.variogram(gstat_variogram,vgm(70000,"Sph",40,20000))
  
  
  ## attempt 2
  plot(r)
  r_lonlat <- load_layers("BO_calcite", datadir = rasterdir, equalarea=FALSE)
  rp <- randomPoints(r_lonlat, sampleSize, lonlatCorrection = TRUE)
  dists <- spDistsN1(rp, rp[1,], longlat = TRUE)
  v <- r_lonlat[cellFromXY(r_lonlat, rp)]
  plot(dists, v)
  hist(v)
}

## semi-variogram (spherical) -> range spatial-autocorrelation

calc_corr_matrix_quad <- function(x, fname, quad = FALSE) {
  if(quad) {
    stack_quad <- x ^ 2
    names(stack_quad) <- paste0(names(x), "_quadratic")
    corr <- faster_pearson(stack(x, stack_quad))
  } else {
    corr <- faster_pearson(x)
  }
  saveRDS(corr, paste0(statsdir, "/corr/", fname, ".rds"))
}

# for(fname in c("pearson_corr_marine_quad", "pearson_corr_terrestrial_quad")) {
#   corr_quad <- readRDS(paste0(statsdir, "/corr/", fname, ".rds"))
#   colnames(corr_quad[[1]]) <- sub("\xb2", "_quadratic", colnames(corr_quad[[1]]))
#   rownames(corr_quad[[1]]) <- sub("\xb2", "_quadratic", rownames(corr_quad[[1]]))
#   saveRDS(corr_quad, paste0(statsdir, "/corr/", fname, ".rds"))
# }


calc_all_correlation_matrices <- function(terrestrial = FALSE, marine = FALSE, freshwater = FALSE, new_rasters = NULL, quad = FALSE) {
#   calc_correlation_matrix <- function(rasterstack, fname) {
#     stats <- layerStats(rasterstack, 'pearson', na.rm=TRUE)
#     correlations <- stats$`pearson correlation coefficient`
#     saveRDS(correlations, fname)
#   }
  if(marine) {
    marine_stack <- load_layers(list_layers(terrestrial=F,marine=T), equalarea = TRUE)
    if(!is.null(new_rasters)){
      marine_stack <- stack(marine_stack, new_rasters)
    }
    #calc_correlation_matrix(marine_stack, paste0(statsdir, "/pearson_corr_marine.rds"))
    calc_corr_matrix_quad(marine_stack, "pearson_corr_marine_quad", quad)
  }
  if(terrestrial) {
    terrestrial_stack <- load_layers(list_layers(terrestrial=T,marine=F), equalarea = TRUE)
    if(!is.null(new_rasters)){
      terrestrial_stack <- stack(terrestrial_stack, new_rasters)
    }
    #calc_correlation_matrix(terrestrial_stack, paste0(statsdir, "/pearson_corr_terrestrial_quad.rds"))
    calc_corr_matrix_quad(terrestrial_stack, "pearson_corr_terrestrial_quad", quad)
  }
  if(freshwater) {
    freshwater_stack <- load_layers(list_layers(terrestrial=F,marine=F, freshwater=T), equalarea = TRUE)
    if(!is.null(new_rasters)){
      freshwater_stack <- stack(freshwater_stack, new_rasters)
    }
    #calc_correlation_matrix(terrestrial_stack, paste0(statsdir, "/pearson_corr_terrestrial_quad.rds"))
    calc_corr_matrix_quad(freshwater_stack, "pearson_corr_freshwater_quad", quad)
  }
  ## TODO implement: Engler, J. O., & Rodder, D. (2012). Disentangling interpolation and extrapolation uncertainties in ecologial niche models : a novel visualization technique for the spatial variation of predictor variable colinearity. Biodiversity Informatics, 8, 30–40.
}
#calc_all_correlation_matrices(marine=T)
#calc_all_correlation_matrices(terrestrial=T)
# calc_all_correlation_matrices(marine=TRUE, new_rasters = raster("../../derived/BO_shoredistance.tif"))
# layers <- read.csv2("data-raw/layers.csv", stringsAsFactors = FALSE)
# envirem <- stack(paste0("../../derived/envirem/", layers[layers$dataset_code=="ENVIREM","layer_code"], ".tif"))
# calc_all_correlation_matrices(terrestrial=T, marine=F, new_rasters = envirem)
# bo2 <- stack(list.files("../../derived/biooracle2/", "[.]tif", full.names=TRUE))
# calc_all_correlation_matrices(terrestrial=FALSE, marine=TRUE, new_rasters = bo2)

combine_data_frame <- function(a, b) {
  r <- as.data.frame(a)
  r[, colnames(b)] <- NA
  r[rownames(b), colnames(b)] <- b
  return(r)
}

get_all_correlations <- function(){
  marine_correlations <- readRDS( paste0(statsdir, "/corr/pearson_corr_marine_quad.rds"))[[1]]
  terrestrial_correlations <- readRDS( paste0(statsdir, "/corr/pearson_corr_terrestrial_quad.rds"))[[1]]
  freshwater_correlations <- readRDS( paste0(statsdir, "/corr/pearson_corr_freshwater_quad.rds"))
  all <- combine_data_frame(marine_correlations, terrestrial_correlations)
  all <- combine_data_frame(all, freshwater_correlations)
  return(all)
}

faster_pearson <- function(x, cachesize=20) { ## always na.rm=TRUE
  mat <- sdmpredictors:::pearson_correlation_matrix(x, cachesize)
  covar <- list(mat)
  names(covar) <- c("pearson correlation coefficient")
  return(covar)
}
faster_pearson <- cmpfun(faster_pearson)
#faster_pearson(x=load_layers(list_layers()[1:4,], rasterdir))

pearson_speed_experiments <- function() {

  parallel_pearson <- function(x) {
  #   library(foreach)
  #   library(doParallel)
  #   
  #   cl<-makeCluster(3)
  #   registerDoParallel(cl)
  #   
  #   asSample <- TRUE
  #   nl <- nlayers(x)
  #   n <- ncell(x)
  #   mat <- matrix(NA, nrow=nl, ncol=nl)
  #   colnames(mat) <- rownames(mat) <- names(x)
  #   
  #   means <- c()
  #   sds <- c()
  #   for (i in 1:nl) {
  #     vals <- values(raster(x, layer=i))
  #     means[i] <- mean(vals, na.rm=TRUE)
  #     sds[i] <- sd(vals, na.rm=TRUE)
  #   }
  #   x <- x - means
  #   
  #   for(i in 1:nl) {
  #     iR <- values(raster(x, layer=i))
  #     strt<-Sys.time()
  #     corr <- foreach(j=i:nl) %dopar% {
  #       library(raster)
  #       if (i == j) {
  #         v <- 1
  #       }
  #       else {
  #         jR <- values(raster(x, layer=j))
  #         r <- (iR * jR)
  #         v <- sum(r, na.rm=TRUE) / ((n - sum(is.na(r)) - asSample) * sds[i] * sds[j])
  #       }
  #       #mat[j,i] <- mat[i,j] <- v
  #       v
  #     }
  #     print(Sys.time()-strt)
  #     print(corr)
  #     ## copy result to matrix
  #     for (j in i:nl) {
  #       mat[j,i] <- mat[i,j] <- corr[[j]]
  #     }
  #   }
  #   covar <- list(mat)
  #   names(covar) <- c("pearson correlation coefficient")
  #   stopCluster(cl)
  #   return(covar)
  }
  #parallel_pearson(x=load_layers(list_layers()[1:5,], rasterdir))
  #

  
  prepare_pearson <- function(x) {
    strt<-Sys.time()
    
    means <- c()
    sds <- c()
    for (i in 1:nlayers(x)) {
      vals <- values(raster(x, layer=i))
      means[i] <- mean(vals, na.rm=TRUE)
      sds[i] <- sd(vals, na.rm=TRUE)
    }
    x <- x - means
    print("startup pearson finished")
    print(Sys.time()-strt)
    return(list(x=x,sds=sds))
  }
  
  faster_pearson2 <- function(x, sds) { ## always na.rm=TRUE
    strt <- Sys.time()
    
    asSample <- TRUE
    nl <- nlayers(x)
    n <- ncell(x)
    mat <- matrix(NA, nrow=nl, ncol=nl)
    colnames(mat) <- rownames(mat) <- names(x)
    for (i in 1:nl) {
      mat[i,i] <- 1
    }
    for (i in seq(from = 1, to = nl-1, by = 4)) {
      i2 <- i+1
      i3 <- i+2
      i4 <- i+3
      iR <- values(raster(x, layer = i))
      if(i2 <= nl) {
        i2R <- values(raster(x, layer = i2))
        mat <- pearson_corr2(sds, mat, i, i2, iR, i2R)
      }
      if(i3 <= nl) {
        i3R <- values(raster(x, layer = i3))
        mat <- pearson_corr2(sds, mat, i, i3, iR, i3R)
        mat <- pearson_corr2(sds, mat, i2, i3, i2R, i3R)
      }
      if(i4 <= nl) {
        i4R <- values(raster(x, layer = i4))
        mat <- pearson_corr2(sds, mat, i, i4, iR, i4R)
        mat <- pearson_corr2(sds, mat, i2, i4, i2R, i4R)
        mat <- pearson_corr2(sds, mat, i3, i4, i3R, i4R)
      }
      if(i4 < nl) {
        for (j in (i4+1):nl) {
          jR <- values(raster(x, layer=j))
          mat <- pearson_corr2(sds, mat, i, j, iR, jR)
          mat <- pearson_corr2(sds, mat, i2, j, i2R, jR)
          mat <- pearson_corr2(sds, mat, i3, j, i3R, jR)
          mat <- pearson_corr2(sds, mat, i4, j, i4R, jR)
        }
      }
    }
    covar <- list(mat)
    names(covar) <- c("pearson correlation coefficient")
    
    print("correlation time")
    print(Sys.time()-strt)
    return(covar)
  }
  faster_pearson2 <- cmpfun(faster_pearson2)
  
  l <- prepare_pearson(x=load_layers(list_layers()[1:10,], rasterdir))
  x <- l[[1]]
  sds <- l[[2]]
  faster_pearson2(x, sds)
  #faster_pearson2(x=load_layers(list_layers()[1:10,], rasterdir))
  #faster_pearson(x=load_layers(list_layers()[1:10,], rasterdir))
  
  
  faster_pearson3 <- function(x, sds) { ## always na.rm=TRUE
    strt <- Sys.time()
    
    asSample <- TRUE
    nl <- nlayers(x)
    n <- ncell(x)
    mat <- matrix(NA, nrow=nl, ncol=nl)
    colnames(mat) <- rownames(mat) <- names(x)
    for (i in 1:nl) {
      mat[i,i] <- 1
    }
    previousJ <- NA
    previousR <- c()
    for(i in 1:(nl-1)) {
      loopstart<-Sys.time()
      iR <- values(raster(x, layer=i))
      mat[i,i] <- 1
      for(j in (i+1):nl) {
        if(is.na(mat[i,j])) {
          jR <- values(raster(x, layer=j))
          mat <- pearson_corr(sds, mat, i, j, iR, jR)
          
          #         r <- (iR * jR)
          #         v <- sum(r, na.rm=TRUE) / ((n - sum(is.na(r)) - asSample) * sds[i] * sds[j])
          #         mat[j,i] <- mat[i,j] <- v
          
          ## cache and re-use previous raster (less GC, less disk seeks)
          if(!is.null(previousR) && !is.na(previousJ) && is.na(mat[j,previousJ])) {
            #           r <- (previousR * jR)
            #           v <- sum(r, na.rm=TRUE) / ((n - sum(is.na(r)) - asSample) * sds[i] * sds[j])
            #           mat[j,previousJ] <- mat[previousJ,j] <- v
            
            mat <- pearson_corr(sds, mat, previousJ, j, previousR, jR)
          }
          previousJ <- j
          previousR <- jR
        }
      }
      #print(Sys.time()-loopstart)
    }
    covar <- list(mat)
    names(covar) <- c("pearson correlation coefficient")
    
    print("correlation time")
    print(Sys.time()-strt)
    return(covar)
  }
  faster_pearson3 <- cmpfun(faster_pearson3)

  
  faster_pearson4 <- function(x, sds) { ## always na.rm=TRUE
    strt <- Sys.time()
    
    asSample <- TRUE
    nl <- nlayers(x)
    n <- ncell(x)
    mat <- matrix(NA, nrow=nl, ncol=nl)
    colnames(mat) <- rownames(mat) <- names(x)
    for (i in 1:nl) {
      mat[i,i] <- 1
    }
    previousJ <- NA
    previousR <- c()
    for (i in seq(from = 1, to = nl-1, by = 4)) {
      i2 <- i+1
      i3 <- i+2
      i4 <- i+3
      iR <- values(raster(x, layer = i))
      if(i2 <= nl) {
        i2R <- values(raster(x, layer = i2))
        mat <- pearson_corr(sds, mat, i, i2, iR, i2R)
      }
      if(i3 <= nl) {
        i3R <- values(raster(x, layer = i3))
        mat <- pearson_corr(sds, mat, i, i3, iR, i3R)
        mat <- pearson_corr(sds, mat, i2, i3, i2R, i3R)
      }
      if(i4 <= nl) {
        i4R <- values(raster(x, layer = i4))
        mat <- pearson_corr(sds, mat, i, i4, iR, i4R)
        mat <- pearson_corr(sds, mat, i2, i4, i2R, i4R)
        mat <- pearson_corr(sds, mat, i3, i4, i3R, i4R)
      }
      if(i4 < nl) {
        for (j in (i4+1):nl) {
          jR <- values(raster(x, layer=j))
          mat <- pearson_corr(sds, mat, i, j, iR, jR)
          if(i2 <= nl)
            mat <- pearson_corr(sds, mat, i2, j, i2R, jR)
          if(i3 <= nl)
            mat <- pearson_corr(sds, mat, i3, j, i3R, jR)
          if(i4 <= nl)
            mat <- pearson_corr(sds, mat, i4, j, i4R, jR)
          if(!is.null(previousR) && !is.na(previousJ) && is.na(mat[j,previousJ])) {
            mat <- pearson_corr(sds, mat, previousJ, j, previousR, jR)
          }
          previousJ <- j
          previousR <- jR
        }
      }
    }
    covar <- list(mat)
    names(covar) <- c("pearson correlation coefficient")
    
    print("correlation time")
    print(Sys.time()-strt)
    return(covar)
  }
  faster_pearson4 <- cmpfun(faster_pearson4)
  
  
  faster_pearson5 <- function(x, sds) { ## always na.rm=TRUE
    strt <- Sys.time()
    
    asSample <- TRUE
    nl <- nlayers(x)
    n <- ncell(x)
    mat <- matrix(NA, nrow=nl, ncol=nl)
    colnames(mat) <- rownames(mat) <- names(x)
    for (i in 1:nl) {
      mat[i,i] <- 1
    }
    for (i in seq(from = 1, to = nl-1, by = 5)) {
      is <- 0:4+i
      v <- lapply(X = is[is <= nl], function(i) { values(raster(x, layer=i)) })
      if(length(v) > 1) {
        for( vi in 1:(length(v)-1)) {
          for (vj in (vi+1):length(v)) {
            mat <- pearson_corr(sds, mat, is[vi], is[vj], v[[vi]], v[[vj]])
          }
        }
      }
      i2 <- i+1
      i3 <- i+2
      i4 <- i+3
      i5 <- i+4
      iR <- values(raster(x, layer = i))
      if(i2 <= nl) {
        i2R <- values(raster(x, layer = i2))
        mat <- pearson_corr2(sds, mat, i, i2, iR, i2R)
      }
      if(i3 <= nl) {
        i3R <- values(raster(x, layer = i3))
        mat <- pearson_corr2(sds, mat, i, i3, iR, i3R)
        mat <- pearson_corr2(sds, mat, i2, i3, i2R, i3R)
      }
      if(i4 <= nl) {
        i4R <- values(raster(x, layer = i4))
        mat <- pearson_corr2(sds, mat, i, i4, iR, i4R)
        mat <- pearson_corr2(sds, mat, i2, i4, i2R, i4R)
        mat <- pearson_corr2(sds, mat, i3, i4, i3R, i4R)
      }
      if(i5 <= nl) {
        i5R <- values(raster(x, layer = i5))
        mat <- pearson_corr2(sds, mat, i, i5, iR, i5R)
        mat <- pearson_corr2(sds, mat, i2, i5, i2R, i5R)
        mat <- pearson_corr2(sds, mat, i3, i5, i3R, i5R)
        mat <- pearson_corr2(sds, mat, i4, i5, i4R, i5R)
      }
      if(i5 < nl) {
        for (j in (i5+1):nl) {
          jR <- values(raster(x, layer=j))
          mat <- pearson_corr2(sds, mat, i, j, iR, jR)
          mat <- pearson_corr2(sds, mat, i2, j, i2R, jR)
          mat <- pearson_corr2(sds, mat, i3, j, i3R, jR)
          mat <- pearson_corr2(sds, mat, i4, j, i4R, jR)
          mat <- pearson_corr2(sds, mat, i5, j, i5R, jR)
        }
      }
    }
    covar <- list(mat)
    names(covar) <- c("pearson correlation coefficient")
    
    print("correlation time")
    print(Sys.time()-strt)
    return(covar)
  }
  faster_pearson5 <- cmpfun(faster_pearson5)
  
  
  faster_pearson6 <- function(x, sds, cachesize=5) { ## always na.rm=TRUE
    strt <- Sys.time()
    
    asSample <- TRUE
    nl <- nlayers(x)
    n <- ncell(x)
    mat <- matrix(NA, nrow=nl, ncol=nl)
    colnames(mat) <- rownames(mat) <- names(x)
    for (i in 1:nl) {
      mat[i,i] <- 1
    }
    for (i in seq(from = 0, to = nl-2, by = cachesize)) {
      indexes <- i + 1:cachesize
      v <- lapply(X = indexes[indexes <= nl], function(i) { values(raster(x, layer=i)) })
      if(length(v) > 1) {
        for( vi in 1:(length(v)-1)) {
          for (vj in (vi+1):length(v)) {
            mat <- pearson_corr(sds, mat, indexes[vi], indexes[vj], v[[vi]], v[[vj]])
          }
        }
      }
      
      if(max(is) < nl) {
        for (j in (max(is)+1):nl) {
          jR <- values(raster(x, layer=j))
          for(vi in 1:length(v)) {
            mat <- pearson_corr(sds, mat, is[vi], j, v[[vi]], jR)
          }
        }
      }
    }
    covar <- list(mat)
    names(covar) <- c("pearson correlation coefficient")
    
    print("correlation time")
    print(Sys.time()-strt)
    return(covar)
  }
  faster_pearson6 <- cmpfun(faster_pearson6)
  
}

cor_sample_test <- function() {
  layers <- list_layers()[1:5,2]
  truth <- layers_correlation(layers)[1:5,1:5]
  env <- load_layers(layers, equalarea = TRUE)
  values <- getValues(env)
  values <- values[complete.cases(values),]
  #values <- lapply(1:5, function(i) getValues(raster(env, )))
  cor(values[,1], values[,2], method = "pearson")
  for(n in c(100000)) {
    r <- sapply(1:50, function(s) {
      set.seed(s)
      i <- sample(1:nrow(values), n)
      cor(values[i,2], values[i,3], method = "pearson")})
    print(summary(r))
  }
}

plot_corr <- function(corr_matrix) {
#   palette <- colorRampPalette(c("darkred", "red", "blue", "white", "white", "blue", "red", "darkred"))(n = 799)
#   
#   breaks = c(seq(-1,-0.72,length=100),
#              seq(-0.72,-0.7,length=100),
#              seq(-0.7,-0.699,length=100),
#              seq(-0.699,0.1,length=100),
#              seq(0.1,0.699,length=100),
#              seq(0.699,0.7,length=100),
#              seq(0.7,0.72,length=100), 
#              seq(0.72,1,length=100))

  heatmap.2(abs(corr_matrix),
            #cellnote = corr.matrix,  # same data set for cell labels
            main = "Correlation", # heat map title
            notecol="black",      # change font color of cell labels to black
            density.info="none",  # turns off density plot inside color legend
            trace="none",         # turns off trace lines inside the heat map
            margins =c(12,9)     # widens margins around plot
#             ,col=palette       # use on color palette defined earlier 
#             ,breaks=breaks    # enable color transition at specified limits
            #dendrogram="row",    # no dendrogram
            #Colv="NA"
  )            # turn off column clustering
}
#plot_corr(corr_m5[[1]])

## Machine learning auto-correlation approach experiment
## conclusion it kind of works but don't really know how to interpret the results
## Having difficulties with defining a cut-off point and with generating results at closer distances

direction <- function(origin, destination) {
  d <- destination < origin
  d[origin < destination] <- (-1)
  return(d)
}
eastwest <- function(origin, destination) {
  direction(origin[,1], destination[,1])
}
northsouth <- function(origin, destination) {
  direction(origin[,2], destination[,2])
}
analyze_sac <- function(r, seed, totalpoints=10000, subsetpoints = 200) {
  if(!grepl("[+]proj=longlat [+]datum=WGS84", r@crs@projargs)) {
    stop("raster projection should be WGS84")
  }
#   set.seed(seed)
#   rp <- randomPoints(r, npoints, lonlatCorrection = TRUE)
#   origin <- rp[1,,drop=FALSE]
#   rp <- rp[-1,]
#   dists <- spDistsN1(rp, origin, longlat = TRUE)
#   ew <- eastwest(origin, rp)
#   ns <- northsouth(origin, rp)
#   
#   
#   library(geosphere)
#   br <- geosphere::bearing(origin, rp)
#   brf <- geosphere::finalBearing(origin, rp)
#   
#   v <- r[cellFromXY(r, rp)]
#   
#   df <- data.frame(v,dists,br,brf)
#   
#   plot(dists, v)
#   plot(ew, v)
#   plot(ns, v)
#   plot(br, v)
#   library(scatterplot3d)
#   
#   scatterplot3d(x = br,
#           y = dists,
#           z = v)
#   
#   df <- data.frame(v,dists,br,brf)
#   fit <- glm(v~dists+br+brf+dists,data=df,family=gaussian())
#   summary(fit) # display results
#   confint(fit) # 95% CI for the coefficients
#   exp(coef(fit)) # exponentiated coefficients
#   exp(confint(fit)) # 95% CI for exponentiated coefficients
#   predict(fit, type="response") # predicted values
#   residuals(fit, type="deviance") # residuals
  
  
  get_rmse <- function(df, plots=FALSE) {
    library(glmnet)
    m <- as.matrix(df[,2:ncol(df)])
    m2 <- m^2
    colnames(m2) <- paste(colnames(m), "^2")
    ## TODO add product features ???
    
    x <- cbind(m, m2)
    y <- df[,1]
    
    train=sample(1:nrow(x), nrow(x)/2)
    test=(-train)
    ytest=y[test]
    
    fit <- glmnet(x[train,], y[train])
    cvfit = cv.glmnet(x[train,], y[train])
    
    if(plots) {
      plot(fit)
      plot(fit, xvar = "dev", label = TRUE)
      plot(cvfit)
    }
    
    #predict(cvfit)
    pred <- predict(cvfit, newx = x[test,], s = "lambda.min")
    rmse <- mean((pred-ytest)^2)
    pred1se <- predict(cvfit, newx = x[test,], s = "lambda.1se")
    rmse1se <- sqrt(mean((pred1se-ytest)^2))
    print(paste("maxdist:", max(df$dists), "rmse:", rmse, "; rmse 1se:", rmse1se))
    
    ## TODO make it work for even shorter distances
    
    if(plots) {
      dists <- df$dists
      plot(dists[train], y[train])
      points(dists[test], ytest, col="red")
      points(dists[test], pred1se, col="darkgreen")
      plot(ytest, pred1se)
      plot(ytest, pred)
      plot(dists[test], sqrt((pred-ytest)^2))
      hist(pred-ytest)
      plot(dists[test], pred-ytest)
    }
    return(rmse)
  }
  
  random_points <- function(r, npoints) {
    if(!grepl("[+]proj=longlat [+]datum=WGS84", r@crs@projargs)) {
      stop("raster projection should be WGS84")
    }
    
    ## sample(c(1, 2, 3), size = 100, replace = TRUE, prob = c(0.1, 0.5, 0.4))
    sample_cols <- function(npoints) {
      sample.int(ncol(r), npoints, replace = TRUE)
    }
    sample_rows <- function(npoints) {
      a <- area(crop(r, extent(r, 1, nrow(r), 1, 1)))
      row_prob <- values(a) / sum(values(a))
      sample.int(nrow(r), npoints, replace = TRUE, prob = row_prob)
    }
    
    get_cells <- function(npoints) {
      unique(cellFromRowCol(r, sample_rows(npoints), sample_cols(npoints)))
    }
    
    isna <- is.na(values(r))
    
    cells <- c()
    for(i in 1:5) {
      tcells <- get_cells(npoints)
      tcells <- tcells[!isna[tcells]]
      cells <- unique(c(cells, tcells))
      if(length(cells) >= npoints) {
        break
      }
    }
    cells <- cells[seq_len(npoints)]
    return(xyFromCell(r, cells))
  }
  
  get_df_builder <- function(seed, r, totalpoints=10000, subsetpoints=200) {
    library(geosphere)
    
    set.seed(seed)
    
    #rp <- randomPoints(r, totalpoints, lonlatCorrection = TRUE)
    rp <- random_points(r, totalpoints)
    v <- r[cellFromXY(r, rp)]
    
    get_subsetdf <- function(maxdist) {
      i <- sample(seq_len(nrow(rp)), 1)
      origin <- rp[i,,drop=FALSE]
      destination <- rp[-i,]
      
      dists <- spDistsN1(destination, origin, longlat = TRUE)
      filter <- dists < maxdist
      
      if(sum(filter) > subsetpoints) {
        subfilter <- sample(1:sum(filter), subsetpoints)
        
        dists <- dists[filter][subfilter]
        vtemp <- v[-i][filter][subfilter]
        destination <- destination[filter,][subfilter,]
        
        br <- geosphere::bearing(origin, destination)
        brf <- geosphere::finalBearing(origin, destination)
        df <- data.frame(v=vtemp,dists,br,brf)
        return(df)
      } else {
        return(c())
      }
    }
    return(get_subsetdf)
  }
  #get_rmse(df,plots=T)
  #get_rmse(df_creator(500), plots=T)
  
  df_creator <- get_df_builder(seed=42,r,totalpoints = totalpoints, subsetpoints = subsetpoints)
  series <- c(seq(200,5000,50))
  rmses <- sapply(series, 
                 function(maxdist) {
                   df <- df_creator(maxdist)
                   if(is.null(df)){
                     df <- df_creator(maxdist)  
                     print(paste("no df for maxdist:", maxdist))
                   }
                   ifelse(!is.null(df), get_rmse(df,plots=FALSE), NA)
                 })
  plot(series, rmses)
}
# analyze_sac(r, seed=42, totalpoints=500000, subsetpoints = 300)
# r_crop <- crop(r, extent(r, 1, nrow(r)/2, 1, ncol(r)/2))
# analyze_sac(r_crop, seed=42, totalpoints=500000, subsetpoints = 300)
# 
# 
# df_creator <- get_df_builder(seed=42,r_crop,totalpoints = totalpoints, subsetpoints = subsetpoints)
# series <- c(seq(50,1000,25))
# rmses <- sapply(series, 
#                 function(maxdist) {
#                   df <- df_creator(maxdist)
#                   if(is.null(df)){
#                     df <- df_creator(maxdist)  
#                     print(paste("no df for maxdist:", maxdist))
#                   }
#                   ifelse(!is.null(df), get_rmse(df,plots=FALSE), NA)
#                 })
# plot(series, rmses)
# get_rmse(df_creator(500), plots=T)
