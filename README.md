[![MIT License](https://img.shields.io/github/license/samuelbosch/blogbits.svg)](https://github.com/lifewatch/sdmpredictors/blob/master/LICENSE.md)
[![Build Status](https://travis-ci.org/lifewatch/sdmpredictors.svg?branch=master)](https://travis-ci.org/lifewatch/sdmpredictors)
[![Coverage Status](http://codecov.io/github/lifewatch/sdmpredictors/coverage.svg?branch=master)](http://codecov.io/github/lifewatch/sdmpredictors?branch=master)
[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/sdmpredictors)](https://CRAN.R-project.org/package=sdmpredictors)

# sdmpredictors: a compilation of species distribution modelling predictors data

An R package to improve the usability of datasets with predictors for species distribution modelling (SDM).

Installation:

    install.packages("sdmpredictors")
    # or for the latest dev version
    devtools::install_github("lifewatch/sdmpredictors")

or with packrat:

    packrat::init()
    devtools::install_github("lifewatch/sdmpredictors")


Example 1: Create SDM for Dictyota diemensis in Australia 
Note that this requires the ZOON, *ggplot2*, *cowplot* and *marinespeed* packages to be installed.

    library(sdmpredictors)
    library(zoon)
    # Inspect the available datasets and layers
    datasets <- list_datasets(terrestrial = FALSE, marine = TRUE)
    View(datasets)
    layers <- list_layers(datasets)
    View(layers)
    # Load equal area rasters and crop with the extent of the Baltic Sea
    layercodes <- c("MS_biogeo05_dist_shore_5m", "MS_bathy_5m", 
                    "BO_sstrange", "BO_sstmean", "BO_salinity")
    env <- load_layers(layercodes, equalarea = TRUE)
    australia <- raster::crop(env, extent(106e5,154e5, -52e5, -13e5))
    plot(australia)
    # Compare statistics between the original and the Australian bathymetry
    View(rbind(layer_stats("MS_bathy_5m"),
               calculate_statistics("Bathymetry Australia", 
                                    raster(australia, layer = 2))))
    # Compare correlations between predictors, globally and for Australia
    prettynames <- list(BO_salinity="Salinity", BO_sstmean="SST (mean)", 
                        BO_sstrange="SST (range)", MS_bathy_5m="Bathymetry",
                        MS_biogeo05_dist_shore_5m = "Shore distance")
    p1 <- plot_corr(layers_correlation(layercodes), prettynames)
    australian_correlations <- pearson_correlation_matrix(australia)
    p2 <- plot_correlation(australian_correlations, prettynames)
    cowplot::plot_grid(p1, p2, labels=c("A", "B"), ncol = 2, nrow = 1)
    print(correlation_groups(australian_correlations))
    # Fetch occurrences and prepare for ZOON
    occ <- marinespeed::get_occurrences("Dictyota diemensis")
    points <- SpatialPoints(occ[,c("longitude", "latitude")],
                            lonlatproj)
    points <- spTransform(points, equalareaproj)
    occfile <- tempfile(fileext = ".csv")
    write.csv(cbind(coordinates(points), value=1), occfile)
    # Create SDM with ZOON
    workflow(
      occurrence = LocalOccurrenceData(
        occfile, occurrenceType="presence",
        columns = c("longitude", "latitude", "value")), 
      covariate = LocalRaster(stack(australia)),
      process = OneHundredBackground(seed = 42),
      model = LogisticRegression,
      output = PrintMap)
    # Layer citations
    print(layer_citations(layercodes))

Example 2: view marine datasets, layers and load a few of them by name

    library(sdmpredictors)
    
    # exploring the marine datasets
    datasets <- list_datasets(terrestrial = FALSE, marine = TRUE)
    View(datasets)
    browseURL(datasets$url[1])
    
    # exploring the layers
    layers <- list_layers(datasets)
    View(layers)
    
    # download specific layers to the current directory
    rasters <- load_layers(c("BO_calcite", "BO_chlomean", "MS_bathy_5m"), datadir = ".")
    
Example 3: looking up statistics and correlations for marine annual layers:
    
    datasets <- list_datasets(terrestrial = FALSE, marine = TRUE)
    layers <- list_layers(datasets)
    
    # filter out monthly layers
    layers <- layers[is.na(layers$month),]
    
    stats <- layer_stats(layers)
    View(stats)
    
    correlations <- layers_correlation(layers)
    View(correlations)
    
    # create groups of layers where no layers in one group 
    # have a correlation > 0.7 with a layer from another group
    groups <- correlation_groups(correlations, max_correlation=0.7)
    
    # inspect groups
    # heatmap plot for larger groups (if gplots library is installed)
    for(group in groups) {
      group_correlation <- as.matrix(correlations[group, group, drop=FALSE])
      if(require(gplots) && length(group) > 4){
        heatmap.2(abs(group_correlation)
                 ,main = "Correlation"
                 ,col = "rainbow"      
                 ,notecol="black"      # change font color of cell labels to black
                 ,density.info="none"  # turns off density plot inside color legend
                 ,trace="none"         # turns off trace lines inside the heat map
                 ,margins = c(12,9)    # widens margins around plot
                 )
      } else {
        print(group_correlation)
      }
    }
    
See the quickstart vignette for more information

    vignette("quickstart", package = "sdmpredictors")
