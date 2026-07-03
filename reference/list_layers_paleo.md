# List the paleo climate layers provided by one or more datasets

`list_layers_paleo` returns information on the paleo climate layers of
one or more datasets.

## Usage

``` r
list_layers_paleo(datasets = c(), model_name = NA, epoch = NA,
  years_ago = NA, terrestrial = NA, marine = NA, freshwater = NA, monthly =
  TRUE, version = NULL)
```

## Arguments

- datasets:

  character vector. Code of the datasets.

- model_name:

  character vector. Paleo climate model name see the `model_name` column
  in the result.

- epoch:

  character vector. Epoch for which you want the paleo layer, e.g.
  `"mid-Holocene", "Last Glacial Maximum"`.

- years_ago:

  integer. Years for which you want the paleo layer, e.g. `6000, 21000`.

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

A dataframe with information on the supported paleo climate layers.

## Details

By default it returns all layers from all datasets, when both marine and
terrestrial are `FALSE` then only layers without land- nor seamasks are
returned.

## See also

[`list_layers`](http://lifewatch.github.io/sdmpredictors/reference/list_layers.md),
[`list_layers_future`](http://lifewatch.github.io/sdmpredictors/reference/list_layers_future.md),
[`list_datasets`](http://lifewatch.github.io/sdmpredictors/reference/list_datasets.md),
[`load_layers`](http://lifewatch.github.io/sdmpredictors/reference/load_layers.md)

## Examples

``` r
# list the first 5 layers
list_layers_paleo()[1:5,]
#>   dataset_code                  layer_code current_layer_code     model_name
#> 1      ENVIREM     ER_annualPET_holo_ccsm4       ER_annualPET     holo_ccsm4
#> 2      ENVIREM ER_annualPET_holo_miroc_esm       ER_annualPET holo_miroc_esm
#> 3      ENVIREM ER_annualPET_holo_mpi_esm_p       ER_annualPET holo_mpi_esm_p
#> 4      ENVIREM      ER_annualPET_lgm_ccsm4       ER_annualPET      lgm_ccsm4
#> 5      ENVIREM  ER_annualPET_lgm_miroc_esm       ER_annualPET  lgm_miroc_esm
#>                  epoch years_ago version
#> 1         mid-Holocene      6000       1
#> 2         mid-Holocene      6000       1
#> 3         mid-Holocene      6000       1
#> 4 Last Glacial Maximum     21000       1
#> 5 Last Glacial Maximum     21000       1
#>                                                                       layer_url
#> 1     https://www.lifewatch.be/sdmpredictors/ER_annualPET_holo_ccsm4_lonlat.tif
#> 2 https://www.lifewatch.be/sdmpredictors/ER_annualPET_holo_miroc_esm_lonlat.tif
#> 3 https://www.lifewatch.be/sdmpredictors/ER_annualPET_holo_mpi_esm_p_lonlat.tif
#> 4      https://www.lifewatch.be/sdmpredictors/ER_annualPET_lgm_ccsm4_lonlat.tif
#> 5  https://www.lifewatch.be/sdmpredictors/ER_annualPET_lgm_miroc_esm_lonlat.tif
# list layer codes for MARSPEC for the mid-Holocene
list_layers_paleo("MARSPEC", epoch = "mid-Holocene")$layer_code
#>  [1] "MS_biogeo08_sss_mean_6kya"     "MS_biogeo09_sss_min_6kya"     
#>  [3] "MS_biogeo10_sss_max_6kya"      "MS_biogeo11_sss_range_6kya"   
#>  [5] "MS_biogeo12_sss_variance_6kya" "MS_biogeo13_sst_mean_6kya"    
#>  [7] "MS_biogeo14_sst_min_6kya"      "MS_biogeo15_sst_max_6kya"     
#>  [9] "MS_biogeo16_sst_range_6kya"    "MS_biogeo17_sst_variance_6kya"
```
