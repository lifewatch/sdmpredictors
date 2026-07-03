# Get the name of future climate layer(s) based on the current climate layer(s)

`get_future_layers` returns information on the future climate layers for
the matching current climate layers.

## Usage

``` r
get_future_layers(current_layer_codes, scenario, year)
```

## Arguments

- current_layer_codes:

  character vector. Code(s) of the current climate layers either as a
  character vector or as the dataframe provided by
  [`list_layers`](http://lifewatch.github.io/sdmpredictors/reference/list_layers.md).

- scenario:

  character vector. Climate change scenario, e.g. `"B1", "A1B", "A2"`.

- year:

  integer. Year for which you want the climate change prediction, e.g.
  `2100, 2200`.

## Value

A dataframe with information on the future layer(s) matching the
provided current layer(s).

## Details

Stops with an exception if no matching future climate layer was found
for one or more of the provided current climate layer codes.

## See also

[`list_layers_future`](http://lifewatch.github.io/sdmpredictors/reference/list_layers_future.md),
[`list_layers`](http://lifewatch.github.io/sdmpredictors/reference/list_layers.md),
[`load_layers`](http://lifewatch.github.io/sdmpredictors/reference/load_layers.md)

## Examples

``` r
future_layers <- get_future_layers(c("BO_salinity", "BO_sstmean"), 
                                   scenario = "B1", year = 2100)
future_layers$layer_code
#> [1] "BO_B1_2100_salinity" "BO_B1_2100_sstmean" 
```
