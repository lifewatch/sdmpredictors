# sdmpredictors: Species Distribution Modeling Predictor Datasets from Around the Web

An R package to improve the usability of datasets with predictors for species distribution modeling (SDM).

Installation:

    devtools::install_github("swbosch/sdmpredictors")

or:

    packrat::init()
    devtools::install_github("swbosch/sdmpredictors")

Example usage:

    library(sdmpredictors)
    
    # exploring the datasets
    datasets <- list_datasets(terrestrial = FALSE, marine = TRUE)
    View(datasets)
    browseUrl(datasets$url[1])
    
    # exploring the layers
    layers <- list_layers(datasets)
    View(layers)
    
    # download specific layers to the current directory or load previously downloaded rasters
    rasters <- load_layers(c("BO_calcite", "BO_chlomean", "MS_bathy_5m"), datadir = ".")
    
    ## filter MARSPEC monthly
    layers <- layers[!grepl("MS_ss[st][01][0-9]", layers$layer_code),]
    
    stats <- layer_stats(layers)
    View(stats)
    
    correlations <- layers_correlation(layers)
    View(correlations)
    
    groups <- correlation_groups(correlations, max_correlation=0.7)
    
    ## inspect groups
    ## heatmap plot for larger groups (if gplots library is installed)
    for(group in groups) {
      group_correlation <- as.matrix(correlations[group,group])
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