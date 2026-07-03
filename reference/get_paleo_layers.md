# Get the name of paleo climate layer(s) based on the current climate layer(s)

`get_paleo_layers` returns information on the future climate layers for
the matching current climate layers.

## Usage

``` r
get_paleo_layers(current_layer_codes, model_name = NA, epoch = NA,
  years_ago = NA)
```

## Arguments

- current_layer_codes:

  character vector. Code(s) of the current climate layers either as a
  character vector or as the dataframe provided by
  [`list_layers`](http://lifewatch.github.io/sdmpredictors/reference/list_layers.md).

- model_name:

  character vector. Paleo climate model name see the `model_name` column
  in the result from
  [`list_layers_paleo`](http://lifewatch.github.io/sdmpredictors/reference/list_layers_paleo.md).

- epoch:

  character vector. Epoch for which you want the paleo layer, e.g.
  `"mid-Holocene", "Last Glacial Maximum"`.

- years_ago:

  integer. Years for which you want the paleo layer, e.g. `6000, 21000`.

## Value

A dataframe with information on the paleo layer(s) matching the provided
current layer(s).

## Details

Stops with an exception if no matching paleo layer was found for one or
more of the provided current climate layer codes.

## See also

[`list_layers_paleo`](http://lifewatch.github.io/sdmpredictors/reference/list_layers_paleo.md),
[`list_layers`](http://lifewatch.github.io/sdmpredictors/reference/list_layers.md),
[`load_layers`](http://lifewatch.github.io/sdmpredictors/reference/load_layers.md)

## Examples

``` r
paleo_layers <- get_paleo_layers("MS_biogeo08_sss_mean_5m", years_ago = 6000)
paleo_layers$layer_code
#> [1] "MS_biogeo08_sss_mean_6kya"
```
