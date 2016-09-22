library(raster)
library(sdmpredictors)

options(sdmpredictors_datadir = "D:/a/projects/predictors/results")

compress_file <- sdmpredictors:::compress_file

prepare_biooracle_bathy <- function() {
  newd <- "D:/a/projects/predictors/derived/"
  for (f in list.files("D:/a/data/BioORACLE_bathy", pattern="[.]asc", full.names=TRUE)) {
    
    r <- raster(f)
    crs(r) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
    
    newf <- paste0("D:/a/projects/predictors/derived", "/BO_", sub("[.]asc", "_lonlat.grd",basename(f)))
    print(newf)
    writeRaster(r, newf, overwrite=T)
    sdmpredictors:::compress_file(newf, newd)
    sdmpredictors:::compress_file(sub("[.]grd", ".gri", newf), newd)
    
    if (nrow(r) == 2160 && ncol(r) == 4320) {
      eares <- 7000 ## similar number of total cells, cells have same x and y res
    } else { stop("undefined ea resolution") }
    r <- projectRaster(r, crs=behrmann, method="ngb", res=eares)
    r[] <- signif(getValues(r), digits = 6) ## limit number of digits to improve compression rate
    newf <- paste0("D:/a/projects/predictors/derived", "/BO_", sub("[.]asc", ".grd",basename(f)))
    print(newf)
    writeRaster(r, newf, overwrite=T)
    sdmpredictors:::compress_file(newf, newd, overwrite=T, remove=T)
    sdmpredictors:::compress_file(sub("[.]grd$", ".gri", newf), newd, overwrite=T, remove=T)
  }
}

prepare <- function() {

  ## Bio-ORACLE from upnpacked rar files
  for (f in list.files("D:/a/projects/predictors/derived/Bio-ORACLE", pattern="[.]asc", full.names=TRUE)) {
    newf <- paste0(dirname(f), "/BO_", sub("[.]asc", ".grd",basename(f)))
    print(newf)
    r <- raster(f)
    crs(r) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
    writeRaster(r, newf, overwrite=T)
    compress_file(newf, "D:/a/projects/predictors/derived/Bio-ORACLE")
    compress_file(sub("[.]grd", ".gri", newf), "D:/a/projects/predictors/derived/Bio-ORACLE")
  }

  ## manually deleted unwanted files
  # MARSPEC
  for (f in list.files("D:/a/projects/predictors/derived/MARSPEC", pattern="[.]gr[di]", full.names=TRUE)) {
    newf <- paste0(dirname(f), "/MS_", basename(f))
    print(newf)
    file.rename(f, newf)
    #r <- raster(f)
    #crs(r) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
    #writeRaster(r, newf, overwrite=T)
    compress_file(newf, "D:/a/projects/predictors/derived/")
    #compress_file(sub("[.]grd", ".gri", newf), "D:/a/projects/predictors/derived/MARSPEC")
  }

  ## convert MARSPEC to real values
  for (f in list.files("D:/a/projects/predictors/derived/MARSPEC", pattern="[.]grd$", full.names=TRUE)) {
    name <- basename(f)
    newf <- paste0(dirname(f), "/real_values/", name)
    r <- raster(f)
    correction <- 100
    n <- gsub("_5m[.]grd$", "", name)
    n <- gsub("^MS_", "", n)
    print(n)
    if (n %in% c("bathy", "biogeo05_dist_shore")) {
    correction <- 1.0
    }
    else if (n %in% c("biogeo03_plan_curvature", "biogeo04_profile_curvature", "biogeo12_sss_variance", "biogeo17_sst_variance")) {
    correction <- 10000.0
    }
    print(correction)
    writeRaster(r / correction, newf, overwrite=T)
    compress_file(newf, "D:/a/projects/predictors/derived/MARSPEC/real_values")
    compress_file(sub("[.]grd", ".gri", newf), "D:/a/projects/predictors/derived/MARSPEC/real_values")
  }
}

marspec_monthly <- function() {
  ## marspec monthly
  for (f in list.files("D:/a/data/marspec/MARSPEC_5m/ascii", pattern="^ss[st].*?[.]asc$", full.names=TRUE)) {
    outd <- "D:/a/projects/predictors/derived/MARSPEC/real_values"
    name <- sub("[.]asc", ".grd",basename(f))
    newf <- paste0(outd, "/MS_",  name)
    print(newf)
    r <- raster(f) / 100 ## apply correction
    crs(r) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
    writeRaster(r, newf, overwrite=T)
    
    compress_file(newf, outd)
    compress_file(sub("[.]grd", ".gri", newf), outd)
  }
}

wgs84 <- sp::CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
behrmann <- sp::CRS("+proj=cea +lon_0=0 +lat_ts=30 +x_0=0 +y_0=0 +datum=WGS84 +ellps=WGS84 +units=m +no_defs") ## same as WGS 84 / NSIDC EASE-Grid Global

project_equalarea <- function() {
  root <- "D:/a/projects/predictors/derived/"
  oldd <- paste0(root, "latest_version")
  newd <- paste0(root, "new_version")
  for (f in list.files(oldd, pattern="[.]gz$", full.names=TRUE)) {
    decompress_file(filename = f, outputdir = oldd, overwrite=T, remove=F)
  }
  for (f in list.files(oldd, pattern="[.]grd$", full.names=TRUE)) {
    layername <- sub("[.]grd$", "", x = basename(f))
    r <- raster(f)
    
    newf <- paste0(newd, "/", basename(f))
    names(r) <- layername
    
    lonlatf <- sub("[.]grd$", "_lonlat.grd", newf)
    writeRaster(r, lonlatf, overwrite=T)
    print(lonlatf)
    compress_file(lonlatf, newd, overwrite=T, remove=T)
    compress_file(sub("[.]grd$", ".gri", lonlatf), newd, overwrite=T, remove=T)
    
    # r <- to_equalarea(r)
    # out resolution
    if (nrow(r) == 2160 && ncol(r) == 4320) {
      eares <- 7000 ## similar number of total cells, cells have same x and y res
    } else { error("undefined ea resolution") }
    r <- projectRaster(r, crs=behrmann, method="ngb", res=eares)
    r[] <- signif(getValues(r), digits = 6) ## limit number of digits to improve compression rate
    print(newf)
    writeRaster(r, newf, overwrite=T)
    compress_file(newf, newd, overwrite=T, remove=T)
    compress_file(sub("[.]grd$", ".gri", newf), newd, overwrite=T, remove=T)
  }
}
#project_equalarea()

# rea <- raster("D:\\a\\projects\\predictors\\derived\\new_version\\BO_chlomax_ea.grd")
# #writeRaster(rea, "D:\\a\\projects\\predictors\\derived\\new_version\\BO_chlomax_ea.asc")
# #as.matrix(rea)
# write.table(signif(as.matrix(rea), digits = 6), "D:\\a\\projects\\predictors\\derived\\new_version\\BO_chlomax_ea.csv", 
#             sep = ";", row.names = F, col.names = F, na="")
# 

# rea <- raster("D:\\a\\projects\\predictors\\derived\\new_version\\BO_calcite.grd")
# (4320*2160) / (2108*4972)
# 1846*4352
# 2457*5799

worldclim <- function() {
  oldwd <- getwd()
  
  setwd("D:/temp")
  wc_layers <- c("tmin", "tmax", "tmean", "prec", "alt", "bio")
  
  ##s <- s / 10 ## temp / 10 (what with derived layers related to temp in bio)
#   s <- getData(name="worldclim", var="bio", res=5)
#   s <- getData(name="worldclim", var="tmean", res=5)
#   s <- getData(name="worldclim", var="tmin", res=5)
#   s <- getData(name="worldclim", var="tmax", res=5)
#   s <- getData(name="worldclim", var="prec", res=5)
#   s <- getData(name="worldclim", var="alt", res=5)
   
  outdir <- "D:/a/projects/predictors/derived/worldclim"
  for (f in list.files("D:/temp/wc5", ".bil$", full.names = TRUE)) {
    print(f)
    newf <- paste0(outdir, "/WC_", sub("[.]bil$", ".grd", basename(f)))
    lonlatf <- sub("[.]grd$", "_lonlat.grd", newf)
    
    r <- raster(f)
    
    ## convert to true values
    if(grepl("tm[eainx]*[0-9]+", names(r)) || 
       names(r) %in% paste0("bio", c(1,2,5,6,7,8,9,10,11))) {
      print(paste(names(r), "divided by 10"))
      r <- r / 10
    } else if(names(r) %in% c("bio3", "bio4")) {
      print(paste(names(r), "divided by 100"))
      r <- r / 100
    }
    
    names(r) <- sub("[.]grd$", "", x = basename(newf))
    crs(r) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
    writeRaster(r, lonlatf, overwrite = TRUE)
    
    print(lonlatf)
    compress_file(lonlatf, outdir, overwrite=T, remove=T)
    compress_file(sub("[.]grd$", ".gri", lonlatf), outdir, overwrite=T, remove=T)
    
    newf <- paste0(outdir, "/WC_", sub("[.]bil", ".grd", basename(f)))
    
    # out resolution
    if (ncol(r) == 4320) {
      eares <- 7000 ## similar number of total cells, cells have same x and y res
    } else { error("undefined ea resolution") }
    r <- projectRaster(r, crs=behrmann, method="ngb", res=eares)
    r[] <- signif(getValues(r), digits = 6) ## limit number of digits to improve compression rate
    print(newf)
    writeRaster(r, newf, overwrite=T)
    compress_file(newf, outdir, overwrite=T, remove=T)
    compress_file(sub("[.]grd$", ".gri", newf), outdir, overwrite=T, remove=T)
  }
  setwd(oldwd)
}
#worldclim()


# standardize_rasters <- function(outdir) {
#   for (layer in list_layers()$layer_code) {
#     z <- function(r, outdir) {
#       r <- raster(r, 1) # pull out of stack
#       name <- sub("[.]grd", "", basename(r@file@name))
#       print(name)
#       newf <- paste0(outdir, name, "_z.grd")
#       
#       r <- (r - cellStats(r, "mean")) / cellStats(r, "sd")
#       
#       writeRaster(r, newf, overwrite=T)
#       sdmpredictors:::compress_file(newf, outdir, overwrite=T, remove=T)
#       sdmpredictors:::compress_file(sub("[.]grd$", ".gri", newf), outdir, overwrite=T, remove=T)
#     }
#     r <- load_layers(layer, equalarea = TRUE)
#     z(r, "D:/a/projects/predictors/derived/standardized/")
#     r <- load_layers(layer, equalarea = FALSE)
#     z(r, "D:/a/projects/predictors/derived/standardized/")
#   }
# }

convert_grd_tif <- function(outdir, overwrite = FALSE) {
  for(layer in list_layers()$layer_code) {
    for(equalarea in c("", "_lonlat")) {
      r <- raster(paste0("../../results/",layer, equalarea, ".grd"))
      name <- sub("[.]grd", "", basename(r@file@name))
      print(name)
      
      r[] <- signif(getValues(r), digits = 6)
      write_tif(r, name, outdir)
    }
  }
}

convert_tif <- function(outdir, overwrite = FALSE) {
  for(layer in list_layers()$layer_code) {
    for(equalarea in c(TRUE, FALSE)) {
      r <- load_layers(layer, equalarea = equalarea)
      r <- raster(r, 1) # pull out of stack
      name <- sub("[.]grd", "", basename(r@file@name))
      print(name)
      
      r[] <- signif(getValues(r), digits = 6)
      write_tif(r, name, outdir)
    }
  }
}
# convert_tif("../../derived/tif/")
is_wholenumber <- function(x, tol = .Machine$double.eps^0.5)  abs(x - round(x)) < tol
in_range <- function(min, min_value, max_value, max) min <= min_value & max_value <= max


write_tif <- function(r, name, outdir) {
  datatype <- "FLT4S"
  predictor <- "PREDICTOR=3"
  
  v <- getValues(r)  
  if(all(is_wholenumber(na.omit(v)))) {
    predictor <- "PREDICTOR=2"
    mn <- min(v, na.rm = TRUE)
    mx <- max(v, na.rm = TRUE)
    if(mn >= 0) {
      if(mx < 256) {
        datatype <- "INT1U"
      } else if (mx < 65534) {
        datatype <- "INT2U"
      } else if (mx < 4294967296) {
        datatype <- "INT4U"
      }
    } else {
      if(in_range(-127, mn, mx, 127)) {
        datatype <- "INT1S"
      } else if(in_range(-32767, mn, mx, 32767)) {
        datatype <- "INT2S"
      } else if(in_range(-2147483647, mn, mx, -2147483647)) {
        datatype <- "INT4S"
      }
    }
  }
  newf <- paste0(outdir, name, ".tif")
  tifoptions <- c("COMPRESS=DEFLATE", predictor, "ZLEVEL=9", "NUM_THREADS=3")
  writeRaster(r, newf, options = tifoptions, datatype = datatype, overwrite = FALSE)
}

project_raster <- function(r, name, outdir) {
  if (nrow(r) == 2160 && ncol(r) == 4320) {
    eares <- 7000 ## similar number of total cells, cells have same x and y res
  } else { error("undefined ea resolution") }
  r <- projectRaster(r, crs=behrmann, method="ngb", res=eares)
  r[] <- signif(getValues(r), digits = 5) ## limit number of digits to improve compression rate
  write_tif(r, name, outdir)
}

prepare_future_biooracle <- function() {
  outdir <- "../../derived/future/"
  for(scenario in c("A1B", "A2", "B1")) {
    for(year in c(2100, 2200)) {
      indir <- paste0("D:/a/data/BioOracle_scenarios/", scenario, "/", year, "/")
      for(l in list.files(indir, "[.]grd", full.names = TRUE)) {
        r <- raster(l)
        crs(r) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
        names(r) <- sub("_", "", names(r))
        write_tif(r, paste("BO", scenario, year, names(r), "lonlat", sep="_"), outdir)
        project_raster(r, paste("BO", scenario, year, names(r), sep="_"), outdir)
      }
    }
  }
}
# prepare_future_biooracle()

prepare_paleo_marspec <- function() {
  outdir <- "../../derived/paleo/"
  scale <- list(bathy_5m = 1,
                biogeo01_5m = 100, biogeo02_5m = 100,
                biogeo03_5m = 10000, biogeo04_5m = 10000,
                biogeo05_5m = 1, biogeo06_5m = 10,
                biogeo07_5m = 10000, biogeo08_5m = 100,
                biogeo09_5m = 100, biogeo10_5m = 100,
                biogeo11_5m = 100, biogeo12_5m = 10000,
                biogeo13_5m = 100, biogeo14_5m = 100,
                biogeo15_5m = 100, biogeo16_5m = 100,
                biogeo17_5m = 10000)
  suffix <- c("aspect_EW", "aspect_NS", "plan_curvature", "profile_curvature", "dist_shore", "bathy_slope", "concavity", 
             "sss_mean", "sss_min", "sss_max", "sss_range", "sss_variance",
             "sst_mean", "sst_min", "sst_max", "sst_range", "sst_variance")
  for(dir in c("6kya", "21kya", "21kya_adjCCSM", "21kya_noCCSM")) {
    for(f in list.files(paste0("D:/a/data/marspec_paleo/tif/", dir), "[.]tif$", full.names = TRUE)) {
      r <- raster(f)
      crs(r) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
      
      name <- names(r)
      scalefactor <- scale[[name]]
      
      name <- sub("_5m$", "", names(r))
      if(grepl("^biogeo", name)) {
        name <- paste0(name, "_", suffix[as.integer(sub("biogeo", "", name))])
      }
      name <- paste0("MS_", name, "_", dir)
      print(name)
      r[] <- signif(getValues(r) / scalefactor, digits = 6)
      write_tif(r, paste(name, "lonlat", sep="_"), outdir)
      project_raster(r, name, outdir)
    }
  }
  
}
# prepare_paleo_marspec()

# writeRaster(r, "D:/temp/BO_salinity_A1B_2100_jpeg.tif", options = c("COMPRESS=JPEG", "JPEG_QUALITY=100"), overwrite = FALSE)
# writeRaster(r, "D:/temp/BO_salinity_A1B_2100_deflate_9_p3.tif", options = c("COMPRESS=DEFLATE", "PREDICTOR=3", "ZLEVEL=9", "NUM_THREADS=3"), overwrite = FALSE)
# writeRaster(r, "D:/temp/BO_salinity_A1B_2100_lzw_9_p3.tif", options = c("COMPRESS=LZW", "PREDICTOR=3", "ZLEVEL=9", "NUM_THREADS=3"), overwrite = FALSE)
# writeRaster(r, "D:/temp/BO_salinity_A1B_2100_packbits.tif", options = c("COMPRESS=PACKBITS", "PREDICTOR=3", "ZLEVEL=9", "NUM_THREADS=3"), overwrite = FALSE)
# writeRaster(r, "D:/temp/BO_salinity_A1B_2100_zip_p3_z9.tif", options = c("COMPRESS=DEFLATE", "PREDICTOR=3", "ZLEVEL=9"), datatype = "FLT4S", overwrite = FALSE)
# writeRaster(r, "D:/temp/BO_salinity_A1B_2100_zip_p3_z9_tiled.tif", options = c("COMPRESS=DEFLATE", "PREDICTOR=3", "ZLEVEL=9", "TILED=YES"), datatype = "FLT4S", overwrite = FALSE)
# writeRaster(r, "D:/temp/BO_salinity_A1B_2100_zip_p3_z6_tiled.tif", options = c("COMPRESS=DEFLATE", "PREDICTOR=3", "ZLEVEL=6", "TILED=YES"), datatype = "FLT4S", overwrite = FALSE)
# writeRaster(r, "D:/temp/BO_salinity_A1B_2100_zip_p2_z6_tiled.tif", options = c("COMPRESS=DEFLATE", "PREDICTOR=2", "ZLEVEL=6", "TILED=YES"), datatype = "FLT4S", overwrite = FALSE)
# writeRaster(r, "D:/temp/BO_salinity_A1B_2100_zip_p2_z9_tiled.tif", options = c("COMPRESS=PACKBITS", "PREDICTOR=2", "ZLEVEL=9", "TILED=YES"), datatype = "FLT4S", overwrite = FALSE)
# writeRaster(r, "D:/temp/BO_salinity_A1B_2100_zip_p3_z6.tif", options = c("COMPRESS=DEFLATE", "PREDICTOR=3", "ZLEVEL=6", "TILED=NO"), datatype = "FLT4S", overwrite = FALSE)
# writeRaster(r, "D:/temp/BO_salinity_A1B_2100_zip_p2_z6.tif", options = c("COMPRESS=DEFLATE", "PREDICTOR=2", "ZLEVEL=6", "TILED=NO"), datatype = "FLT4S", overwrite = FALSE)
# writeRaster(r, "D:/temp/BO_salinity_A1B_2100_zip_p2_z9.tif", options = c("COMPRESS=DEFLATE", "PREDICTOR=2", "ZLEVEL=9", "TILED=NO"), datatype = "FLT4S", overwrite = FALSE)
# writeRaster(r, "D:/temp/BO_salinity_A1B_2100_zip_packbits.tif", options = c("COMPRESS=PACKBITS"), datatype = "FLT4S", overwrite = FALSE)
# writeRaster(r, "D:/temp/BO_salinity_A1B_2100_lzw_p3_z9_tiled.tif", options = c("COMPRESS=LZW", "PREDICTOR=3", "ZLEVEL=9", "TILED=YES"), datatype = "FLT4S", overwrite = FALSE)


# prepare_paleo_marspec()
# convert_grd_tif("../../derived/tif/")
# prepare_future_biooracle()


bench <- function(layer) {
  t <- sapply(1:10, function(i) system.time({getValues(raster(layer))})[1])
  plot(t, main=layer)
  list(layer=t)
}
# l1 <- bench("D:/temp/MS_bathy_5m_test.tif")
# l2 <- bench("D:/temp/MS_bathy_5m_test_p2.tif")
# l3 <- bench("D:/temp/MS_bathy_5m_test_p1.tif")
# l4 <- bench("D:/temp/MS_bathy_5m_test_lzw.tif")
# 
# 
# s1 <- bench("D:/temp/BO_salinity_A1B_2100_zip.tif")
# s2 <- bench("D:/temp/BO_salinity_A1B_2100_lzw_p3_z9_tiled.tif")
# s3 <- bench("D:/temp/BO_salinity_A1B_2100_zip_p3_z9_tiled.tif")
# s4 <- bench("D:/temp/BO_salinity_A1B_2100_zip_p3_z9.tif")
# t <- bench("D:/temp/BO_salinity_A1B_2100_zip_packbits.tif")
# t <- bench("D:/temp/BO_salinity_A1B_2100_zip_p2_z9.tif")
# t <- bench("D:/temp/BO_salinity_A1B_2100_zip_p2_z6.tif")
# t <- bench("D:/temp/BO_salinity_A1B_2100_zip_p3_z9.tif")
# t <- bench("D:/temp/BO_salinity_A1B_2100_zip_p3_z6.tif")

