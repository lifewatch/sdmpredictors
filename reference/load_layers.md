# Load layers

Method to load rasters from disk or from the internet. By default a
RasterStack is returned but this is only possible When all rasters have
the same spatial extent and resolution.

## Usage

``` r
load_layers(layercodes, equalarea=FALSE, rasterstack=TRUE, 
  datadir=NULL)
```

## Arguments

- layercodes:

  character vector or dataframe. Layer_codes of the layers to be loaded
  or dataframe with a "layer_code" column.

- equalarea:

  logical. If `TRUE` then layers are loaded with a Behrmann cylindrical
  equal-area projection
  ([`equalareaproj`](http://lifewatch.github.io/sdmpredictors/reference/equalareaproj.md)),
  otherwise unprojected
  ([`lonlatproj`](http://lifewatch.github.io/sdmpredictors/reference/lonlatproj.md)).
  Default is `FALSE`.

- rasterstack:

  logical. If `TRUE` (default value) then the result is a
  [`stack`](https://rdrr.io/pkg/raster/man/stack.html) otherwise a list
  of rasters is returned.

- datadir:

  character. Directory where you want to store the data. If `NULL` is
  passed (default) then the `sdmpredictors_datadir` option is read. To
  set this run
  `options(sdmpredictors_datadir="<your preferred directory>")` in every
  session or add it to your .RProfile.

## Value

RasterStack or list of raster

## See also

[`list_layers`](http://lifewatch.github.io/sdmpredictors/reference/list_layers.md),
[`layer_stats`](http://lifewatch.github.io/sdmpredictors/reference/layer_stats.md),
[`layers_correlation`](http://lifewatch.github.io/sdmpredictors/reference/layers_correlation.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# warning using tempdir() implies that data will be downloaded again in the 
# next R session
env <- load_layers("BO_calcite", datadir = tempdir())
} # }
```
