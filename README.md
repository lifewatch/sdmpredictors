[![Build Status](https://travis-ci.org/samuelbosch/sdmpredictors.svg?branch=master)](https://travis-ci.org/samuelbosch/sdmpredictors)
[![Coverage Status](https://coveralls.io/repos/samuelbosch/sdmpredictors/badge.svg?branch=master&service=github)](https://coveralls.io/github/samuelbosch/sdmpredictors?branch=master)

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
    
## License

The MIT License (MIT)

Copyright (c) 2015 Samuel

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
