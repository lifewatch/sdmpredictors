# Translate file name from sdmpredictors to bio-oracle

Translate file name from sdmpredictors to bio-oracle

## Usage

``` r
sdm_to_bo(sdm)
```

## Arguments

- sdm:

  the name of the sdm string

## Value

A string with the name of the same file in bio-oracle

## Examples

``` r
sdm_to_bo("BO2_tempmean_ss.tif")
#> [1] "Present.Surface.Temperature.Mean.tif.zip"
sdm_to_bo("BO21_RCP26_2050_curvelltmax_bdmax.tif")
#> [1] "2050AOGCM.RCP26.Benthic.Max.Depth.Current.Velocity.Lt.max.BOv2_1.tif.zip"
```
