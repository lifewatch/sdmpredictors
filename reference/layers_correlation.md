# Gives the Pearson correlation between layers

`layers_correlations` returns the Pearson product-moment correlation
coefficient (Pearson's r) for every combination of the give layercodes.
The correlation between a terrestrial and a marine layer has been set to
`NA`.

## Usage

``` r
layers_correlation(layercodes = c())
```

## Arguments

- layercodes:

  character vector or dataframe. Codes of the layers, you want the
  correlation matrix of, as a character vector or a dataframe with a
  "layer_code" column. With the default empty vector the correlation
  between all layers is returned.

## Value

A dataframe with the Pearson product-moment correlation coefficients.

## See also

[`list_layers`](http://lifewatch.github.io/sdmpredictors/reference/list_layers.md)` `[`layer_stats`](http://lifewatch.github.io/sdmpredictors/reference/layer_stats.md)` `[`correlation_groups`](http://lifewatch.github.io/sdmpredictors/reference/correlation_groups.md)` `[`plot_correlation`](http://lifewatch.github.io/sdmpredictors/reference/plot_correlation.md)

## Examples

``` r
# correlation of the first 10 layers
layers_correlation()[1:10,1:10]
#>               BO_calcite   BO_chlomax  BO_chlomean  BO_chlomin BO_chlorange
#> BO_calcite    1.00000000  0.396639051  0.461932611  0.47908017   0.27472469
#> BO_chlomax    0.39663905  1.000000000  0.912505114  0.71757426   0.93753070
#> BO_chlomean   0.46193261  0.912505114  1.000000000  0.91075416   0.72764396
#> BO_chlomin    0.47908017  0.717574261  0.910754165  1.00000000   0.43044004
#> BO_chlorange  0.27472469  0.937530703  0.727643957  0.43044004   1.00000000
#> BO_cloudmax   0.01186976  0.062216689  0.047982923  0.02447058   0.06840751
#> BO_cloudmean -0.03459517  0.008067028  0.004488046 -0.00619996   0.01355159
#> BO_cloudmin  -0.05677585 -0.027173265 -0.024417737 -0.02605302  -0.02220186
#> BO_damax      0.45886642  0.843088069  0.764031226  0.59133188   0.79723753
#> BO_damean     0.55272492  0.827089496  0.839946041  0.71820912   0.71312691
#>              BO_cloudmax BO_cloudmean BO_cloudmin   BO_damax  BO_damean
#> BO_calcite    0.01186976 -0.034595166 -0.05677585 0.45886642 0.55272492
#> BO_chlomax    0.06221669  0.008067028 -0.02717326 0.84308807 0.82708950
#> BO_chlomean   0.04798292  0.004488046 -0.02441774 0.76403123 0.83994604
#> BO_chlomin    0.02447058 -0.006199960 -0.02605302 0.59133188 0.71820912
#> BO_chlorange  0.06840751  0.013551587 -0.02220186 0.79723753 0.71312691
#> BO_cloudmax   1.00000000  0.915586672  0.77631593 0.22998205 0.18412390
#> BO_cloudmean  0.91558667  1.000000000  0.94594660 0.13893701 0.09882120
#> BO_cloudmin   0.77631593  0.945946600  1.00000000 0.06319287 0.02937793
#> BO_damax      0.22998205  0.138937009  0.06319287 1.00000000 0.95694561
#> BO_damean     0.18412390  0.098821204  0.02937793 0.95694561 1.00000000
layers_correlation(c("BO_calcite", "MS_bathy_5m"))
#>             BO_calcite MS_bathy_5m
#> BO_calcite   1.0000000   0.2485496
#> MS_bathy_5m  0.2485496   1.0000000
layers_correlation(c("BO_calcite", "MS_bathy_5m"))
#>             BO_calcite MS_bathy_5m
#> BO_calcite   1.0000000   0.2485496
#> MS_bathy_5m  0.2485496   1.0000000
```
