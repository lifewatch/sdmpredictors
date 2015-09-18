[![MIT License](https://img.shields.io/github/license/samuelbosch/blogbits.svg)](https://github.com/samuelbosch/sdmpredictors/blob/master/LICENSE.md)
[![Build Status](https://travis-ci.org/samuelbosch/sdmpredictors.svg?branch=master)](https://travis-ci.org/samuelbosch/sdmpredictors)
[![Coverage Status](http://codecov.io/github/samuelbosch/sdmpredictors/coverage.svg?branch=master)](http://codecov.io/github/samuelbosch/sdmpredictors?branch=master)


# sdmpredictors: a compilation of species distribution modeling predictors data

An R package to improve the usability of datasets with predictors for species distribution modeling (SDM).

Installation:

    devtools::install_github("samuelbosch/sdmpredictors")

or with packrat:

    packrat::init()
    devtools::install_github("samuelbosch/sdmpredictors")

Example usage:

    # Example 1: view marine datasets, layers and load a few of them by name
    
    library(sdmpredictors)
    
    # exploring the marine datasets
    datasets <- list_datasets(terrestrial = FALSE, marine = TRUE)
    View(datasets)
    browseURL(datasets$url[1])
    
    # exploring the layers
    layers <- list_layers(datasets)
    View(layers)
    
    # download specific layers to the current directory or load previously downloaded rasters
    rasters <- load_layers(c("BO_calcite", "BO_chlomean", "MS_bathy_5m"), datadir = ".")
    
    # Example: 2 looking up statistics and correlations for marine annual layers:
    
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
