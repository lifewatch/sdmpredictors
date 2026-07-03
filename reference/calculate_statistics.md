# Calculate statistics for a given raster.

Method used to calculate the statistics of all layers. It can be re-used
to calculate statistics for a cropped version of the rasters.

## Usage

``` r
calculate_statistics(layercode, raster)
```

## Arguments

- layercode:

  character. Name of the layer.

- raster:

  RasterLayer. The raster you want to calculate statistics for.

## Value

A dataframe with the layercode and all basic statistics.

## See also

[`layer_stats`](http://lifewatch.github.io/sdmpredictors/reference/layer_stats.md)

## Examples
