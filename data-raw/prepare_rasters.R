library(raster)
library(sdmpredictors)

options(sdmpredictors_datadir = "D:/a/projects/predictors/results")

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


standardize_rasters <- function() {
  for (layer in list_layers()$layer_code) {
    z <- function(r, outdir) {
      r <- raster(r, 1) # pull out of stack
      name <- sub("[.]grd", "", basename(r@file@name))
      print(name)
      newf <- paste0(outdir, name, "_z.grd")
      
      r <- (r - cellStats(r, "mean")) / cellStats(r, "sd")
      
      writeRaster(r, newf, overwrite=T)
      sdmpredictors:::compress_file(newf, outdir, overwrite=T, remove=T)
      sdmpredictors:::compress_file(sub("[.]grd$", ".gri", newf), outdir, overwrite=T, remove=T)
    }
    r <- load_layers(layer, equalarea = TRUE)
    z(r, "D:/a/projects/predictors/derived/standardized/")
    r <- load_layers(layer, equalarea = FALSE)
    z(r, "D:/a/projects/predictors/derived/standardized/")
  }
}
#standardize_rasters()