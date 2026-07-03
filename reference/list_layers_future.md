# List the future climate layers provided by one or more datasets

`list_layers_future` returns information on the future climate layers of
one or more datasets.

## Usage

``` r
list_layers_future(datasets = c(), scenario = NA, year = NA,
  terrestrial = NA, marine = NA, freshwater = NA, monthly = TRUE, version =
  NULL)
```

## Arguments

- datasets:

  character vector. Code of the datasets.

- scenario:

  character vector. Climate change scenario, e.g. `"B1", "A1B", "A2"`.

- year:

  integer. Year for which you want the climate change prediction, e.g.
  `2100, 2200`.

- terrestrial:

  logical. When `TRUE` (default), then datasets that only have
  terrestrial data (seamasked) are returned.

- marine:

  logical. When `TRUE` (default), then datasets that only have marine
  data (landmasked) are returned.

- freshwater:

  logical. When `TRUE`, then datasets that only have freshwater data are
  returned.

- monthly:

  logical. When `FALSE`, then no monthly layers are returned. All annual
  and monthly layers are returned by default.

- version:

  numeric vector. When `NULL` then layers from all versions of datasets
  are returned (default) else layers are filtered by version number.

## Value

A dataframe with information on the supported future climate layers.

## Details

By default it returns all layers from all datasets, when both marine and
terrestrial are `FALSE` then only layers without land- nor seamasks are
returned.

## See also

[`list_layers`](http://lifewatch.github.io/sdmpredictors/reference/list_layers.md),
[`list_layers_paleo`](http://lifewatch.github.io/sdmpredictors/reference/list_layers_paleo.md),
[`list_datasets`](http://lifewatch.github.io/sdmpredictors/reference/list_datasets.md),
[`load_layers`](http://lifewatch.github.io/sdmpredictors/reference/load_layers.md)

## Examples

``` r
# list the first 5 layers
list_layers_future()[1:5,]
#>   dataset_code        layer_code current_layer_code model scenario year version
#> 1    WorldClim WC_bio1_cc26_2050            WC_bio1 CCSM4    rcp26 2050       1
#> 2    WorldClim WC_bio1_cc26_2070            WC_bio1 CCSM4    rcp26 2070       1
#> 3    WorldClim WC_bio1_cc45_2050            WC_bio1 CCSM4    rcp45 2050       1
#> 4    WorldClim WC_bio1_cc45_2070            WC_bio1 CCSM4    rcp45 2070       1
#> 5    WorldClim WC_bio1_cc60_2050            WC_bio1 CCSM4    rcp60 2050       1
#>                                                             layer_url
#> 1 https://www.lifewatch.be/sdmpredictors/WC_bio1_cc26_2050_lonlat.tif
#> 2 https://www.lifewatch.be/sdmpredictors/WC_bio1_cc26_2070_lonlat.tif
#> 3 https://www.lifewatch.be/sdmpredictors/WC_bio1_cc45_2050_lonlat.tif
#> 4 https://www.lifewatch.be/sdmpredictors/WC_bio1_cc45_2070_lonlat.tif
#> 5 https://www.lifewatch.be/sdmpredictors/WC_bio1_cc60_2050_lonlat.tif
# list layer codes for Bio-ORACLE with scenario B1 and year 2100
list_layers_future("Bio-ORACLE", scenario = "B1", year = 2100)$layer_code
#> [1] "BO_B1_2100_salinity" "BO_B1_2100_sstmax"   "BO_B1_2100_sstmean" 
#> [4] "BO_B1_2100_sstmin"   "BO_B1_2100_sstrange"
```
