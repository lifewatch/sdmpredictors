# Groups layers based on the Pearson correlation

`correlation_groups` returns groups of layer codes such as each layer
from one group has an absolute Pearson product-moment correlation
coefficient (Pearson's r) that is smaller than the maximum_correlation
(default 0.7) with each variable in any other group. The correlation
values of quadratic layers are used for creating the groups but only non
quadratic layer codes are returned.

## Usage

``` r
correlation_groups(layers_correlation, max_correlation=0.7)
```

## Arguments

- layers_correlation:

  matrix or dataframe. A square matrix with the layers correlations you
  want to group.

- max_correlation:

  number. The maximum correlation 2 layers may have before they are put
  in the same correlation group.

## Value

A list of vectors with each vector containing the layer codes of one
correlation group.

## References

Dormann, C. F., Elith, J., Bacher, S., Buchmann, C., Carl, G., Carre,
G., ... Lautenbach, S. (2013). Collinearity: a review of methods to deal
with it and a simulation study evaluating their performance. Ecography,
36(1), 027-046. doi:10.1111/j.1600-0587.2012.07348.x Barbet-Massin, M. &
Jetz, W. (2014). A 40-year, continent-wide, multispecies assessment of
relevant climate predictors for species distribution modelling.
Diversity and Distributions, 20(11), 1285-1295. doi:10.1111/ddi.12229

## See also

` `[`layers_correlation`](http://lifewatch.github.io/sdmpredictors/reference/layers_correlation.md)` `[`list_layers`](http://lifewatch.github.io/sdmpredictors/reference/list_layers.md)` `[`layer_stats`](http://lifewatch.github.io/sdmpredictors/reference/layer_stats.md)

## Examples

``` r

corr <- layers_correlation(c("BO_calcite", "BO_damin", "MS_bathy_5m"))
print(corr)
#>             BO_calcite  BO_damin MS_bathy_5m
#> BO_calcite   1.0000000 0.6214034   0.2485496
#> BO_damin     0.6214034 1.0000000   0.4670088
#> MS_bathy_5m  0.2485496 0.4670088   1.0000000
print(correlation_groups(corr, max_correlation=0.6))
#> [[1]]
#>   BO_calcite     BO_damin 
#> "BO_calcite"   "BO_damin" 
#> 
#> [[2]]
#>   MS_bathy_5m 
#> "MS_bathy_5m" 
#> 
```
