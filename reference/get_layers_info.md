# Layer info for specific layer codes

`get_layers_info` returns all detailed information on the current or
future climate layers of one or more datasets.

## Usage

``` r
get_layers_info(layer_codes = c())
```

## Arguments

- layer_codes:

  character vector. Vector with the layer codes of the layers you want
  the full information for. This can also be a dataframe with as column
  `layer_code`.

## Value

A list with four dataframes `common`, `current`, `future` and `paleo`,
the `common` dataframe contains data for all shared columns in the other
three dataframes. The other dataframes contain all detailed information
on the layer(s) matching the layer codes. By default information for all
layers is returned.

## See also

[`list_layers`](http://lifewatch.github.io/sdmpredictors/reference/list_layers.md),
[`list_layers_future`](http://lifewatch.github.io/sdmpredictors/reference/list_layers_future.md),
[`list_layers_paleo`](http://lifewatch.github.io/sdmpredictors/reference/list_layers_paleo.md),
[`load_layers`](http://lifewatch.github.io/sdmpredictors/reference/load_layers.md)

## Examples

``` r
info <- get_layers_info(c("BO_salinity", "BO_B1_2100_salinity"))
info$common
#>         time dataset_code          layer_code
#> 478  current   Bio-ORACLE         BO_salinity
#> 1336  future   Bio-ORACLE BO_B1_2100_salinity
#>                                                    layer_url
#> 478          https://bio-oracle.org/data/1.0/BO_salinity.zip
#> 1336 https://bio-oracle.org/data/1.0/BO_B1_2100_salinity.zip
info$current
#>     dataset_code  layer_code     name
#> 478   Bio-ORACLE BO_salinity Salinity
#>                                                     description terrestrial
#> 478 Salinity indicates the dissolved salt content in the ocean.       FALSE
#>     marine freshwater cellsize_equalarea cellsize_lonlat units
#> 478   TRUE      FALSE               7000      0.08333333   PSS
#>            primary_type primary_spatial_resolution
#> 478 in situ measurement                         ''
#>                                                                                primary_source
#> 478 World Ocean Database (2009) Reference: (Boyer et al. 2009) URL: http://www.nodc.noaa.gov/
#>     start_year start_month start_day end_year end_month end_day
#> 478       1961           1         1     2009        12      31
#>                                  derivation month is_surface version
#> 478 DIVA interpolation (532377 data points)    NA       TRUE      10
#>                                           layer_url
#> 478 https://bio-oracle.org/data/1.0/BO_salinity.zip
info$future
#>      dataset_code          layer_code current_layer_code       model scenario
#> 1336   Bio-ORACLE BO_B1_2100_salinity        BO_salinity UKMO-HadCM3       B1
#>      year version                                               layer_url
#> 1336 2100       1 https://bio-oracle.org/data/1.0/BO_B1_2100_salinity.zip
info$paleo
#> [1] dataset_code       layer_code         current_layer_code model_name        
#> [5] epoch              years_ago          version            layer_url         
#> <0 rows> (or 0-length row.names)
```
