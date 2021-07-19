#' Translate file name from sdmpredictors to bio-oracle
#'
#' @param sdm the name of the sdm string
#'
#' @return A string with the name of the same file in bio-oracle
#' @export
#'
#' @examples 
#' sdm_to_bo("BO2_tempmean_ss.tif")
#' sdm_to_bo("BO21_RCP26_2050_curvelltmax_bdmax.tif")
sdm_to_bo <- function(sdm){
  # Assert
  stopifnot(is.character(sdm))
  # Create empty string
  bo <- ""
  #----------Check Time---------- 
  if(grepl("2100", sdm, fixed = TRUE)){
    bo <- paste0(bo, "2100AOGCM")
  }else if(grepl("2050", sdm, fixed = TRUE)){
    bo <- paste0(bo, "2050AOGCM")
  }else{
    bo <- paste0(bo, "Present")
    # do nothing
  }
  #----------Check RCP---------- 
  if(grepl("RCP26", sdm, fixed = TRUE)){
    bo <- paste(bo, "RCP26", sep = ".")
  }else if(grepl("RCP45", sdm, fixed = TRUE)){
    bo <- paste(bo, "RCP45", sep = ".")
  }else if(grepl("RCP60", sdm, fixed = TRUE)){
    bo <- paste(bo, "RCP60", sep = ".")
  }else if(grepl("RCP85", sdm, fixed = TRUE)){
    bo <- paste(bo, "RCP85", sep = ".")
  }else{
    # do nothing
  }
  #---------- Check Realm-------------
  if(grepl("ss", sdm, fixed = TRUE)){
    bo <- paste(bo, "Surface", sep = ".")
  }else if(grepl("bdmax", sdm, fixed = TRUE)){
    bo <- paste(bo, "Benthic.Max.Depth", sep = ".")
  }else if(grepl("bdmean", sdm, fixed = TRUE)){
    bo <- paste(bo, "Benthic.Mean.Depth", sep = ".")
  }else if(grepl("bdmin", sdm, fixed = TRUE)){
    bo <- paste(bo, "Benthic.Min.Depth", sep = ".")
  }else{
    stop(paste0("An exception was found in Realm at: ", sdm))
  }
  #---------- Check Environmental Variable-------------
  if(grepl("calcite", sdm, fixed = TRUE)){
    bo <- paste(bo, "Calcite", sep = ".")
  }else if(grepl("chlo", sdm, fixed = TRUE)){
    bo <- paste(bo, "Chlorophyll", sep = ".")
  }else if(grepl("cloud", sdm, fixed = TRUE)){
    bo <- paste(bo, "Cloud.cover", sep = ".")
  }else if(grepl("curvel", sdm, fixed = TRUE)){
    bo <- paste(bo, "Current.Velocity", sep = ".")
  }else if(grepl("da", sdm, fixed = TRUE)){
    bo <- paste(bo, "Diffuse.attenuation", sep = ".")
  }else if(grepl("dissox", sdm, fixed = TRUE)){
    bo <- paste(bo, "Dissolved.oxygen", sep = ".")
  }else if(grepl("icecover", sdm, fixed = TRUE)){
    bo <- paste(bo, "Ice.cover", sep = ".")
  }else if(grepl("icethick", sdm, fixed = TRUE)){
    bo <- paste(bo, "Ice.thickness", sep = ".")
  }else if(grepl("iron", sdm, fixed = TRUE)){
    bo <- paste(bo, "Iron", sep = ".")
  }else if(grepl("lightbot", sdm, fixed = TRUE)){
    bo <- paste(bo, "Light.bottom", sep = ".")
  }else if(grepl("nitrate", sdm, fixed = TRUE)){
    bo <- paste(bo, "Nitrate", sep = ".")
  }else if(grepl("par", sdm, fixed = TRUE)){
    bo <- paste(bo, "Par", sep = ".")
  }else if(grepl("ph", sdm, fixed = TRUE)){
    if(grepl("phosphate", sdm, fixed = TRUE)){
      bo <- paste(bo, "Phosphate", sep = ".")
    }else if(grepl("carbonphyto", sdm, fixed = TRUE)){
      bo <- paste(bo, "Phytoplankton", sep = ".")
    }else{
      bo <- paste(bo, "pH", sep = ".")
    }
  }else if(grepl("pp", sdm, fixed = TRUE)){
    bo <- paste(bo, "Primary.productivity", sep = ".")
  }else if(grepl("salinity", sdm, fixed = TRUE)){
    bo <- paste(bo, "Salinity", sep = ".")
  }else if(grepl("silicate", sdm, fixed = TRUE)){
    bo <- paste(bo, "Silicate", sep = ".")
  }else if(grepl("sst", sdm, fixed = TRUE) | grepl("temp", sdm, fixed = TRUE)){
    bo <- paste(bo, "Temperature", sep = ".")
  }else {
    stop(paste0("An exception was found in Environmental variable at: ", sdm))
  }
  #------------Check Variable------------
  # Check lt and variable separated
  if(grepl("lt", sdm, fixed = TRUE)){
    bo <- paste(bo, "Lt", sep = ".")
    if(grepl("max", sdm, fixed = TRUE)){
      bo <- paste(bo, "max", sep = ".")
    }else if(grepl("min", sdm, fixed = TRUE)){
      bo <- paste(bo, "min", sep = ".")
    }else{
      stop(paste0("An exception was found in Variable (Lt) at: ", sdm))
    }
  }else{
    if(grepl("max", sdm, fixed = TRUE)){
      bo <- paste(bo, "Max", sep = ".")
    }else if(grepl("min", sdm, fixed = TRUE)){
      bo <- paste(bo, "Min", sep = ".")
    }else if(grepl("mean", sdm, fixed = TRUE)){
      bo <- paste(bo, "Mean", sep = ".")
    }else if(grepl("range", sdm, fixed = TRUE)){
      bo <- paste(bo, "Range", sep = ".")
    }else{
      stop(paste0("An exception was found in Variable at: ", sdm))
    }
  }
  #------------Check version------------
  if(grepl("BO2", sdm, fixed = TRUE)){
    if(grepl("BO21", sdm, fixed = TRUE)){
      bo <- paste(bo, "BOv2_1", sep = ".")
    }
    # If version 2, do nothing
  }else{
    # If version is 0, let's assign v2.1
    paste(bo, "BOv2_1", sep = ".")
  }
  #------------Add extension------------
  bo <- paste(bo, "tif.zip", sep = ".")
  # END
  return(bo)
}
