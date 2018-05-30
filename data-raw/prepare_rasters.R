library(raster)
library(sdmpredictors)

#options(sdmpredictors_datadir = "D:/a/projects/predictors/results")

compress_file <- sdmpredictors:::compress_file

prepare_layer <- function(layerpath, outputdir, newname, scalefactor = 1)  {
  r <- raster(layerpath)
  if(scalefactor != 1) {
    print(paste(newname, "divided by ", scalefactor))
    r <- r / scalefactor
  }
  crs(r) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
#   newf <- file.path(outputdir, paste0(newname, "_lonlat.grd"))
#   print(newf)
#   writeRaster(r, newf, overwrite=T)
#   sdmpredictors:::compress_file(newf, outputdir)
#   sdmpredictors:::compress_file(sub("[.]grd", ".gri", newf), outputdir)
  write_tif(r, paste0(newname, "_lonlat"), outputdir)
  
  ## BEHRMANN
  if (ncol(r) == 4320 && abs(res(r)-0.0833333) < 0.001) {
    eares <- 7000 ## similar number of total cells, cells have same x and y res
  } else { stop("undefined ea resolution") }
  r <- projectRaster(r, crs=behrmann, method="ngb", res=eares, over=TRUE)

  ## Eckert IV
  # if (ncol(r) == 4320 && abs(res(r)-0.0833333) < 0.001) {
  #   eares <- 7500 ## similar number of total cells, cells have same x and y res
  # } else { stop("undefined ea resolution") }
  # EckertIV <- "+proj=eck4 +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"
  # r <- projectRaster(r, crs=EckertIV, method="ngb", res=eares, over=TRUE)
  
  r[] <- signif(getValues(r), digits = 6) ## limit number of digits to improve compression rate
#   newf <- file.path(outputdir, paste0(newname, ".grd"))
#   print(newf)
#   writeRaster(r, newf, overwrite=T)
#   sdmpredictors:::compress_file(newf, outputdir, overwrite=T, remove=T)
#   sdmpredictors:::compress_file(sub("[.]grd$", ".gri", newf), outputdir, overwrite=T, remove=T)
  write_tif(r, newname, outputdir)
}

# prepare_layer("D:/a/data/BioORACLE_bathy/shoredistance.asc", "D:/a/projects/predictors/derived", "BO_shoredistance")

prepare_biooracle_bathy <- function() {
  ind <- "D:/a/data/BioORACLE_bathy"
  newd <- "D:/a/projects/predictors/derived/"
  for(bathy in c("bathymin", "bathymax", "bathymean")) {
    prepare_layer(file.path(ind, paste0(bathy, ".asc")), 
                  "D:/a/projects/predictors/derived/", 
                  paste0("BO_", bathy))  
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
  newf <- file.path(outdir, paste0(name, ".tif"))
  print(newf)
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

prepare_envirem <- function() {
  ind <- "D:/a/data/ENVIREM"
  newd <- "D:/a/projects/predictors/derived/envirem"
  layerpaths <- list.files(ind, "[.]tif$", full.names = TRUE)
  for(layerpath in layerpaths) {
    p <- sub("[.]tif$", "", basename(layerpath))
    parts <- unlist(strsplit(p, "_5arcmin_"))
    if(parts[1] == "current") {
      newname <- paste0("ER_",parts[2])
    } else {
      newname <- paste0("ER_",parts[2],"_",parts[1])
    }
    print(newname)
    prepare_layer(layerpath, newd, newname)  
  }
}
# prepare_envirem()

prepare_worldclim_paleofuture <- function(prepare_layers=TRUE, file_pattern = NULL) {
  layers <- read.csv2("data-raw/layers.csv", stringsAsFactors = FALSE)
  future <- read.csv2("data-raw/layers_future.csv", stringsAsFactors = FALSE)
  paleo <- read.csv2("data-raw/layers_paleo.csv", stringsAsFactors = FALSE)
  
  wc_layers <- c("tmin", "tmax", "tmean", "prec", "alt", "bio")
  
  outdir <- "D:/a/projects/predictors/derived/worldclim_paleo_future"
  
  gcm_lookup <- list(CC="CCSM4", ME="MPI-ESM-P", MR="MIROC-ESM", HE="HadGEM2-ES")
  
  paleo_info <- list(lgm=list(epoch="Last Glacial Maximum", year = 21000, code="lgm"), 
                     mid=list(epoch="mid-Holocene", year = 6000, code="holo"))
  vartypes <- list(bi="bio", pr="prec", "tx"="tmax", "tn" = "tmin")
  
  for(zip in list.files("D:/a/data/WorldClim/paleo_future", pattern = file_pattern, full.names = TRUE)) {
    print(zip)
    fname <- sub("_5m[.]", ".", basename(zip))
    gcm_code <- toupper(substr(fname, 1, 2))
    gcm <- gcm_lookup[[gcm_code]]
    paleo_code <- substr(fname, 3, 5)
    rasters <- unzip(zip, list = TRUE)
    is_paleo <- paleo_code %in% c("lgm", "mid")
    if(is_paleo) {
      vartype <- substr(fname, 6, 7)
      info <- paleo_info[[paleo_code]]
      modelname <- paste(info$code, gcm, sep = " ")
      info <- data.frame(dataset_code = "WorldClim", layer_code = rep(NA, NROW(rasters)), current_layer_code = NA, 
                         model_name = modelname, epoch = info$epoch, years_ago = info$year)  
      suffix <- substr(fname, 1, 5)
    } else {
      scenario <- paste0("rcp", substr(fname, 3, 4))
      vartype <- substr(fname, 5, 6)
      year <- 2000 + as.integer(substr(fname, 7, 8))
      info <- data.frame(dataset_code = "WorldClim", layer_code = rep(NA, NROW(rasters)), current_layer_code = NA, 
                         model = gcm, scenario = scenario, year = year)
      suffix <- paste0(substr(fname, 1, 4), "_", year)
    }
    wc_temp_dir <- "D:/temp/worldclim"
    for(ri in 1:nrow(rasters)) {
      r <- rasters[ri,]
      print(r$Name)
      name <- sub(tools::file_path_sans_ext(fname), "", tools::file_path_sans_ext(r$Name))
      name <- paste0(vartypes[[vartype]], name)
      
      layer_code <- paste0("WC_", name, "_", suffix)
      info[ri,"layer_code"] <- layer_code
      info[ri,"current_layer_code"] <- paste0("WC_", name)
      
      ## convert to true values
      scalefactor <- 1
      if(grepl("tm[eainx]*[0-9]+", name) || 
         name %in% paste0("bio", c(1,2,5,6,7,8,9,10,11))) {
        scalefactor <- 10
      } else if(name %in% c("bio3", "bio4")) {
        scalefactor <- 100
      }
      outzipfile <- paste0(wc_temp_dir, "/", r$Name)
      outfile <- paste0(outdir, "/", layer_code, "_lonlat.tif")
      if(prepare_layers && !file.exists(outzipfile) && !file.exists(outfile)) {
        path <- unzip(zip, r$Name, exdir = wc_temp_dir)
        on.exit(file.remove(path))
        prepare_layer(path, outdir, layer_code, scalefactor)  
      }
    }
    if(is_paleo) {
      paleo <- rbind(paleo, info[!(info$layer_code %in% paleo$layer_code),])
    } else {
      future <- rbind(future, info[!(info$layer_code %in% future$layer_code),])
    }
  }
  write.csv2(paleo[order(paleo$dataset_code, paleo$layer_code),], "data-raw/layers_paleo.csv", row.names = FALSE)
  write.csv2(future[order(future$dataset_code, future$layer_code),], "data-raw/layers_future.csv", row.names = FALSE)
}
# prepare_worldclim_paleofuture(prepare_layers = TRUE)
## prepare_worldclim_paleofuture(prepare_layers = TRUE, file_pattern = "^cc") # all cc gcm
# prepare_worldclim_paleofuture(prepare_layers = TRUE, file_pattern = "^mr") # all mr gcm
# prepare_worldclim_paleofuture(prepare_layers = TRUE, file_pattern = "^me") # all me gcm
# prepare_worldclim_paleofuture(prepare_layers = TRUE, file_pattern = "^he") # all he gcm

BO2_get_code <- function(f) {
  n1lookup <- list(Chlorophyll="chlo", Iron="iron", Nitrate="nitrate", Phosphate="phosphate", 
                   Phytoplankton="carbonphyto", Salinity="salinity", Silicate="silicate", 
                   Temperature="temp")
  n2lookup <- list(Current.Velocity="curvel", Dissolved.oxygen="dissox", 
                   Light.bottom="lightbot", Primary.productivity="pp",
                   Ice.cover="icecover", Ice.thickness="icethick")
  middlelookup <- list(Lt.max="ltmax", Lt.min="ltmin", 
                       Max="max", Min="min", Mean="mean", Range="range")
  suffixdirs <- list(Benthic_Max_Depth="bdmax", Benthic_Min_Depth="bdmin", 
                     Benthic_Mean_Depth="bdmean", Surface="ss")
  
  n <- sub("[.]tif", "", basename(f))
  parts <- strsplit(n , "[.]")[[1]]
  n1 <- parts[1]
  n2 <- paste(parts[1:2], collapse = ".")
  
  if(n1 %in% names(n1lookup)) {
    prefix <- n1lookup[[n1]]
  } else if (n2 %in% names(n2lookup)){
    prefix <- n2lookup[[n2]]
  } else {
    print(n)
    stop("Unkown prefix")
  }
  middle <- c()
  for(m in names(middlelookup)) {
    if(grepl(m, n, fixed=TRUE)) {
      middle <- middlelookup[[m]]
    }
  }
  suffix <- suffixdirs[[basename(dirname(f))]]
  if(is.null(suffix)) {
    print(f)
    stop("UNknown suffix")
  }
  code <- paste0(prefix,middle,"_",suffix)
  
}

prepare_biooracle2 <- function() {
  layers <- read.csv2("data-raw/layers.csv", stringsAsFactors = FALSE)
  rasters <- list.files("D:/a/data/BioOracle2/Present", "[.]tif$", full.names = TRUE, recursive = TRUE)
  outdir <- "D:/a/projects/predictors/derived/biooracle2"
  for (f in rasters) {
    layercode <- paste0("BO2_",BO2_get_code(f))
    if(length(list.files(outdir, layercode)) < 2 || overwrite) {
      print(c(f,layercode))
      if(NROW(layers[layers$layer_code==layercode,]) != 1) {
        stop("not found in layers.csv")
      }  
      prepare_layer(f, outdir, layercode)
    }
  }
}
# prepare_biooracle2()

prepare_biooracle2_future <- function() {
  layers <- read.csv2("data-raw/layers_future.csv", stringsAsFactors = FALSE)
  for(year in c(2050, 2100)) {
    for(rcp in c("RCP26", "RCP45", "RCP60", "RCP85")) {
      rasters <- list.files(paste0("D:/a/data/BioOracle2/",year,"AOGCM/",rcp), "[.]tif$", full.names = TRUE, recursive = TRUE)
      outdir <- "D:/a/projects/predictors/derived/biooracle2"
      for (f in rasters) {
        current_layer_code <- paste0("BO2_",BO2_get_code(f))
        layercode <- paste0("BO2_",rcp, "_", year, "_", BO2_get_code(f))
        if(NROW(layers[layers$layer_code==layercode,]) != 1) {
          layers <- rbind(layers, data.frame(dataset_code="Bio-ORACLE2",
                                             layer_code=layercode,
                                             current_layer_code=current_layer_code,
                                             model = "AOGCM",
                                             scenario = rcp,
                                             year = year))
        }
        if(length(list.files(outdir, layercode)) < 2 || overwrite) {
          print(c(f,layercode))
          prepare_layer(f, outdir, layercode)
        }
      }
    }
  }
  write.csv2(layers, "data-raw/layers_future.csv", row.names = FALSE)
}
# prepare_biooracle2_future()


 
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

rename_file <- function(path, prefix, suffix = NULL) {
  file <- basename(path)
  dirname <- dirname(path)
  ext <- tools::file_ext(file)
  file <- tools::file_path_sans_ext(file)
  if(!grepl(paste0('^', prefix), file)) {
    file <- paste0(prefix, file)
  }
  if(!is.null(suffix) && !grepl(paste0(suffix, '$'), file)) {
    file <- paste0(file, suffix)
  }
  if(grepl('[_][1-9][_]?', file)) {
    for (a in 1:9) {
      for (b in c('$', '_')) {
        file <- sub(paste0('[_]', a, '[', b, ']'), paste0('_0', a, ifelse(b =='$', '', '_')), file)
      }
    }
  }
  tofile <- paste0(dirname, '/', file, '.', ext)
  if(path != tofile) {
    file.rename(path, tofile)
  }
  tofile
}


prepare_bo2_chlorophyll <- function() {
  dir <- '/Users/samuel/Dropbox (IPOfI)/Final'
  files <- list.files(dir, '[.]tif', full.names = TRUE)
  outputdir <- '~/a/projects/sdmpredictors_dataprep/bo2_chlo'
  for (path in files) {
    lname <- tools::file_path_sans_ext(basename(path))
    parts <- unlist(strsplit(lname, 'Chlorophyll'))
    suffix <- paste0('_chlo', tolower(gsub('[.]', '', parts[2])), '_ss')
    if(parts[1] == 'Present.') {
      layercode <- paste0('BO2', suffix)  
    } else {
      layercode <- paste0('BO2_', paste(rev(toupper(unlist(strsplit(parts[1], '[.]')))), collapse = '_'), suffix)
    }
    statsdir <- "data-raw/stats"
    fname <- paste0(statsdir, "/", layercode, ".rds")
    if(!file.exists(fname)) {
      print(layercode)
      prepare_layer(path, outputdir, layercode)
    
      rasterpath <- file.path(outputdir, paste0(layercode, '.tif'))
      stats <- sdmpredictors:::calculate_statistics(layercode, raster(rasterpath))
      saveRDS(stats, fname)
    }
  }
}

prepare_streams <- function() {
  dir <- '~/a/projects/sdmpredictors_dataprep/streams'
  wgs84dir <- paste0(dir, '/wgs84')
  behrmanndir <- paste0(dir, '/behrmann')
  # rename layers
  wgs84files <- list.files(wgs84dir, '[.]tif', recursive = TRUE, full.names = TRUE)
  wgs84files <- sapply(wgs84files, rename_file, prefix='FW_', suffix='_lonlat')
  behrmannfiles <- list.files(behrmanndir, '[.]tif', recursive = TRUE, full.names = TRUE)
  behrmannfiles <- sapply(behrmannfiles, rename_file, prefix='FW_')
  all <- c(wgs84files, behrmannfiles)
  
  stopifnot(length(unique(basename(all))) == length(all))
  
  for(path in all) {
    outfile <- paste0(dir, '/zoutput/', basename(path))
    if(!file.exists(outfile)) {
      file.copy(path, outfile)
    }
  }
  print("calculate stats")
  statsdir <- "data-raw/stats"
  allfiles <- list.files(paste0(dir, '/zoutput'), '[.]tif$', recursive = TRUE, full.names = TRUE)
  equalareafiles <- Filter(function(path) !grepl('[_]lonlat', path), allfiles)
  for(path in equalareafiles) {
    layercode <- tools::file_path_sans_ext(basename(path))
    print(layercode)
    fname <- paste0(statsdir, "/", layercode, ".rds")
    if(!file.exists(fname)) {
      stats <- sdmpredictors:::calculate_statistics(layercode, raster(path))
      
      saveRDS(stats, fname)
    }
  }
  print("calculate corr")
  corrfile <- paste0(statsdir, "/corr/pearson_corr_freshwater_quad.rds")
  if(!file.exists(corrfile)) {
    x <- raster::stack(equalareafiles)
    corr <- sdmpredictors::pearson_correlation_matrix(x, 10, same_mask = TRUE)
    saveRDS(corr, corrfile)
  }
}

check_bathy <- function() {
  x <- load_layers(c('BO2_tempmean_ss', 'BO2_chlomean_ss', 'BO_bathymean', 'BO2_carbonphytomean_ss', 'BO2_ppmean_ss', 'BO2_salinitymean_ss'))
  bathy <- raster(x, layer=3)
  sst <- raster(x, layer=1)
  bathyna <- is.na(values(bathy))
  sstna <- is.na(values(sst))
  nobathy <- bathyna & !sstna
  nosst <- sstna & !bathyna
  sum(nosst)
  sum(nobathy)
  
  # TODO CONTINUE HERE
}

generate_shoredistance <- function() {
  # min, mean, max
  # distance to the four corners, middle point 
  
  template <- raster(sdmpredictors::load_layers('BO2_tempmean_ss'), layer=1)
  template[!is.na(values(template))] <- 32768
  cells <- which(!is.na(values(template)))
  center<- cbind(raster::xyFromCell(template, cells), cell=cells)
  halfres <- res(template)/2
  corners <- rbind(cbind(x=center[,1] - halfres, y=center[,2] + halfres, cell=cells),
                   cbind(x=center[,1] - halfres, y=center[,2] - halfres, cell=cells),
                   cbind(x=center[,1] + halfres, y=center[,2] + halfres, cell=cells),
                   cbind(x=center[,1] + halfres, y=center[,2] - halfres, cell=cells))
  corners[,1] <- round(corners[,1], digits=8)
  corners[,2] <- round(corners[,2], digits=8)
  xy <- rbind(center, corners)
  names(xy) <- c('decimalLongitude', 'decimalLatitude', 'cell')
  
  
  opt <- options(obistools_xylookup_url="http://127.0.0.1:8000/lookup")
  on.exit(options(opt))
  
  shoredist <- obistools::lookup_xy(xy, shoredistance = TRUE, grids=FALSE, areas = FALSE)
  library(dplyr)
  minmaxmean <- cbind(cell=xy[,'cell'], shoredist) %>% 
    group_by(cell) %>% summarise(min(shoredistance), mean(shoredistance), max(shoredistance))
  
  # TODO CONTINUE HERE
}

pointinpolygon <- function() {
  # def load_points(cur, points):
  #   tmptable = "tmp" + str(uuid.uuid4()).replace("-", "")
  #   cur.execute("""CREATE TABLE {0}(id INTEGER);
  #               SELECT AddGeometryColumn('{0}', 'geom', 4326, 'POINT', 2);
  #               CREATE INDEX {0}_geom_gist ON {0} USING gist(geom);""".format(tmptable))
  #   f = points_to_file(points)
  #   cur.copy_from(f, tmptable, columns = ('id', 'geom'))
  #   return tmptable
  library(RPostgreSQL)
  library(readr)
  library(tibble)
  conn <- dbConnect(dbDriver("PostgreSQL"), dbname = 'xylookup', host='localhost', user = 'postgres', password = 'postgres')
  dbExecute(conn, paste0("DROP TABLE IF EXISTS test_jorge;"))
  dbExecute(conn, paste0("CREATE TABLE test_jorge (id INTEGER); ", 
                         "SELECT AddGeometryColumn('test_jorge', 'geom', 4326, 'POINT', 2);",
                         "CREATE INDEX test_jorge_geom_gist ON test_jorge USING gist(geom);"))
  
  # txt = "\n".join(["{}\tSRID=4326;POINT({} {})'".format(idx, xy[0], xy[1]) for idx, xy in enumerate(points)])
  points <- data_frame(id=1:1000000, wkt=paste0('SRID=4326;POINT(', runif(1000000, -12, -8), ' ', runif(1000000, 36, 42), ')'))
  # tmp <- tempfile('jorgepoints', fileext = '.txt')
  tmp <- '/Users/samuel/Public/jorgepoints.txt'
  write_delim(points, tmp, delim = '\t', col_names=FALSE)
  # system(paste0('chmod a+r ', tmp))
  
  dbSendQuery(conn, paste0("COPY test_jorge FROM '", tmp, "';"))
  
  
  #system.time({ onland <- dbGetQuery(conn, "SELECT pts.id FROM test_jorge pts LEFT JOIN water_polygons0_00005 all_water ON ST_Intersects(all_water.geom, pts.geom) WHERE all_water.geom IS NULL") })
  #system.time({ onland2 <- dbGetQuery(conn, "SELECT pts.id FROM test_jorge pts LEFT JOIN water_polygons0_00005 all_water ON ST_DWithin(all_water.geom, pts.geom, 0) WHERE all_water.geom IS NULL") })
  #system.time({ onland3 <- dbGetQuery(conn, "SELECT pts.id FROM test_jorge pts LEFT JOIN water_polygons0_00005 all_water ON ST_Within(all_water.geom, pts.geom) WHERE all_water.geom IS NULL") })
  
  
  # l <- st_read("PG:'dbname=xylookup host=localhost port=5432 user=postgres password=postgres'", "water_polygons0_00005")
  
  # pgsql2shp -f "/Users/samuel/Public/waterpoly4326" -h localhost -u postgres -P postgres xylookup water_polygons0_00005
  
  # pgsql2shp -f "/Users/samuel/Public/waterpoly3857" -h localhost -u postgres -P postgres xylookup "SELECT id, st_transform(geom, 3857) as geom, fid FROM water_polygons0_00005"
  
  
  # install.packages('rredis')
   
  
  # Tile 38
  
  # p1 = st_point(c(7,52))
  # p2 = st_point(c(-30,20))
  # sfc = st_sfc(p1, p2, crs = 4326)
  
  # tile38-server -d /usr/local/var/tile38/data
  # Connect tile38-cli
  
  
  
  # water <- sf::st_read("/Users/samuel/Public/waterpoly3857.shp")
  # water_geoms <- st_combine(water$geometry)
  # sf::st_write(water_geoms, "/Users/samuel/Public/water_multi.geojson")
  # water_geojson <- geojsonio::geojson_json(water_geoms)
  
  land <- sf::st_read("/Users/samuel/Public/GSHHS_3857.shp")
  #land_geojson <- geojsonio::geojson_json(land)
  
  
  library(rredis)
  redisConnect(host='127.0.0.1', port = 9851, nodelay = FALSE)
  
  #result = client.execute_command('SET' SET fleet truck POINT 33.32 115.423')
  
  redisCmd('WITHIN', 'fleet', 'FENCE', 'OBJECT', water_geojson)
  
  redisCmd('SET', 'fleet', 'p1', 'point', '33.5122', '-112.2692')
  
  
  redisClose()
}
