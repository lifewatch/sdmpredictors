# Calculate the Pearson correlation coefficient matrix for a rasterstack

Calculate the Pearson correlation coefficient matrix for a rasterstack

## Usage

``` r
pearson_correlation_matrix(x, cachesize = 20, same_mask = FALSE)
```

## Arguments

- x:

  RasterStack. The stack of rasters you want to calculate the Pearson
  correlation coefficient matrix for. This can be obtained by calling
  [`load_layers`](http://lifewatch.github.io/sdmpredictors/reference/load_layers.md).

- cachesize:

  integer. For how many rasters should the values be kept in local
  memory. By default this is set to 20, a parameter which works
  reasonably well on a windows computer with 8GB RAM.

- same_mask:

  logical. Whether we can assume that the mask is the same for all
  layers (same NA values), default is `FALSE`.

## Value

A correlation matrix.

## See also

[`layers_correlation`](http://lifewatch.github.io/sdmpredictors/reference/layers_correlation.md)` `[`plot_correlation`](http://lifewatch.github.io/sdmpredictors/reference/plot_correlation.md)` `[`load_layers`](http://lifewatch.github.io/sdmpredictors/reference/load_layers.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# calculate correlation between SST and salinity in the Baltic Sea

# warning using tempdir() implies that data will be downloaded again in the
# next R session
x <- load_layers(c("BO_sstmax", "BO_salinity"), datadir = tempdir())
e <- extent(13, 31, 52, 66)
baltics <- crop(x, e)
print(pearson_correlation_matrix(baltics))
} # }
```
