# Gives basic layer statistics

`layer_stats` returns basic statistics (minimum, q1, median, q3,
maximum, median absolute deviation (mad), mean, standard deviation (sd))
for each given layercode.

## Usage

``` r
layer_stats(layercodes = c())
```

## Arguments

- layercodes:

  character vector or dataframe. Codes of the layers you want the basic
  statistics of as a character vector or a dataframe with a "layer_code"
  column. With the default empty vector all statistics are returned.

## Value

A dataframe with basic statistics about each given layercode.

## See also

[`list_layers`](http://lifewatch.github.io/sdmpredictors/reference/list_layers.md)
[`layers_correlation`](http://lifewatch.github.io/sdmpredictors/reference/layers_correlation.md)

## Examples

``` r
# layer stats for the first 10 layers
layer_stats()[1:10,]
#>              layer_code minimum     q1  median     q3 maximum       mad
#> 1  BO_A1B_2100_salinity   0.000 33.061 33.6320 34.555  45.000  1.132706
#> 2    BO_A1B_2100_sstmax  -1.030 16.398 27.9700 32.147  41.650  7.518266
#> 3   BO_A1B_2100_sstmean  -1.610 11.756 23.4620 29.258  35.342 10.367822
#> 4    BO_A1B_2100_sstmin  -1.807  8.609 19.9210 26.416  32.846 11.478290
#> 5  BO_A1B_2100_sstrange   0.767  4.477  6.0940  8.235  33.487  2.722054
#> 6  BO_A1B_2200_salinity   0.000 32.856 33.4700 34.414  45.000  1.183115
#> 7    BO_A1B_2200_sstmax  -0.401 17.804 28.7400 32.797  42.752  7.288459
#> 8   BO_A1B_2200_sstmean  -1.523 12.851 24.1270 29.825  36.705 10.221043
#> 9    BO_A1B_2200_sstmin  -1.810  9.556 20.4845 26.898  33.148 11.283328
#> 10 BO_A1B_2200_sstrange   1.399  4.602  6.2310  8.660  32.117  2.902931
#>         mean        sd     moran        geary
#> 1  33.581528  3.062836 0.9659977 7.967063e-04
#> 2  24.008438  9.946540 0.9906802 3.888232e-05
#> 3  20.332794 10.295028 0.9902515 2.269467e-05
#> 4  17.349216 10.309791 0.9906726 3.242276e-05
#> 5   6.659221  3.064490 0.9900109 4.131083e-04
#> 6  33.424420  3.125564 0.9664247 7.847520e-04
#> 7  24.811906  9.908373 0.9906745 4.283967e-05
#> 8  21.009479 10.251172 0.9900848 2.335164e-05
#> 9  17.922208 10.262336 0.9902781 3.550761e-05
#> 10  6.889698  3.165707 0.9903447 4.372453e-04
layer_stats(c("BO_calcite", "MS_bathy_5m"))
#>       layer_code     minimum         q1     median         q3   maximum
#> 29    BO_calcite  5.0000e-05  6.600e-05  9.400e-05  1.860e-04  0.055976
#> 2425 MS_bathy_5m -1.0493e+04 -4.865e+03 -4.082e+03 -2.984e+03 -1.000000
#>               mad          mean           sd     moran       geary
#> 29   5.337360e-05  4.247966e-04 2.640947e-03 0.8130786 0.074806796
#> 2425 1.313584e+03 -3.661049e+03 1.644869e+03 0.9728919 0.009697763
```
