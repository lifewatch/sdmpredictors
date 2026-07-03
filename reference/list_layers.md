# List the current climate layers provided by one or more datasets

`list_layers` returns information on the layers of one or more datasets.

## Usage

``` r
list_layers(datasets=c(), terrestrial = NA, marine = NA, freshwater =
  NA, monthly = TRUE, version = NULL)
```

## Arguments

- datasets:

  character vector. Code of the datasets.

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

A dataframe with information on the supported current climate layers.

## Details

By default it returns all layers from all datasets, when both marine and
terrestrial are `FALSE` then only layers from datasets without land- nor
seamasks are returned. Layers for paleo and future climatic conditions
can be listed with
[`list_layers_paleo`](http://lifewatch.github.io/sdmpredictors/reference/list_layers_paleo.md)
and
[`list_layers_future`](http://lifewatch.github.io/sdmpredictors/reference/list_layers_future.md).
Available paleo and future climate layers for a current climate layer
code can be listed with the functions
[`get_paleo_layers`](http://lifewatch.github.io/sdmpredictors/reference/get_paleo_layers.md)
and
[`get_future_layers`](http://lifewatch.github.io/sdmpredictors/reference/get_future_layers.md).

## See also

[`load_layers`](http://lifewatch.github.io/sdmpredictors/reference/load_layers.md),
[`list_datasets`](http://lifewatch.github.io/sdmpredictors/reference/list_datasets.md),
[`list_layers_future`](http://lifewatch.github.io/sdmpredictors/reference/list_layers_future.md),
[`list_layers_paleo`](http://lifewatch.github.io/sdmpredictors/reference/list_layers_paleo.md),
[`get_future_layers`](http://lifewatch.github.io/sdmpredictors/reference/get_future_layers.md),
[`get_paleo_layers`](http://lifewatch.github.io/sdmpredictors/reference/get_paleo_layers.md)

## Examples

``` r
# list the first 5 layers
list_layers()[1:5,]
#>   dataset_code layer_code                           name
#> 1    WorldClim     WC_alt                       Altitude
#> 2    WorldClim    WC_bio1        Annual mean temperature
#> 3    WorldClim    WC_bio2 Mean diurnal temperature range
#> 4    WorldClim    WC_bio3                  Isothermality
#> 5    WorldClim    WC_bio4        Temperature seasonality
#>                                                               description
#> 1                                                                Altitude
#> 2                                                 Annual mean temperature
#> 3         Mean of the monthly (maximum temperature - minimum temperature)
#> 4 Mean diurnal temperature range (bio2) / Annual temperature range (bio7)
#> 5                       Standard deviation of the annual mean temperature
#>   terrestrial marine freshwater cellsize_equalarea cellsize_lonlat   units
#> 1        TRUE  FALSE      FALSE               7000      0.08333333  meters
#> 2        TRUE  FALSE      FALSE               7000      0.08333333 Celsius
#> 3        TRUE  FALSE      FALSE               7000      0.08333333 Celsius
#> 4        TRUE  FALSE      FALSE               7000      0.08333333 Celsius
#> 5        TRUE  FALSE      FALSE               7000      0.08333333 Celsius
#>                primary_type primary_spatial_resolution
#> 1 Satellite (SRTM), GTOPO30   3 arcsecond, 30 arsecond
#> 2       in situ measurement                         ''
#> 3       in situ measurement                         ''
#> 4       in situ measurement                         ''
#> 5       in situ measurement                         ''
#>                                                                                                                                                                                                                                           primary_source
#> 1  SRTM: CGIAR-CSI, reference: Jarvis, A., H.I. Reuter, A. Nelson, E. Guevara, 2008, Hole-filled SRTM for the globe Version 4, available from the CGIAR-CSI SRTM 90m Database (http://srtm.csi.cgiar.org). GTOPO30: url: https://lta.cr.usgs.gov/GTOPO30
#> 2 Weather station data from multiple sources, see Hijmans, R.J., S.E. Cameron, J.L. Parra, P.G. Jones and A. Jarvis, 2005. Very high resolution interpolated climate surfaces for global land areas. International Journal of Climatology 25: 1965-1978.
#> 3 Weather station data from multiple sources, see Hijmans, R.J., S.E. Cameron, J.L. Parra, P.G. Jones and A. Jarvis, 2005. Very high resolution interpolated climate surfaces for global land areas. International Journal of Climatology 25: 1965-1978.
#> 4 Weather station data from multiple sources, see Hijmans, R.J., S.E. Cameron, J.L. Parra, P.G. Jones and A. Jarvis, 2005. Very high resolution interpolated climate surfaces for global land areas. International Journal of Climatology 25: 1965-1978.
#> 5 Weather station data from multiple sources, see Hijmans, R.J., S.E. Cameron, J.L. Parra, P.G. Jones and A. Jarvis, 2005. Very high resolution interpolated climate surfaces for global land areas. International Journal of Climatology 25: 1965-1978.
#>   start_year start_month start_day end_year end_month end_day
#> 1       1950           1         1     2000         1       1
#> 2       1950           1         1     2000         1       1
#> 3       1950           1         1     2000         1       1
#> 4       1950           1         1     2000         1       1
#> 5       1950           1         1     2000         1       1
#>                                         derivation month is_surface version
#> 1                                               ''    NA       TRUE      10
#> 2               mean, thin-plate smoothing splines    NA       TRUE      10
#> 3         mean range, thin-plate smoothing splines    NA       TRUE      10
#> 4                                               ''    NA       TRUE      10
#> 5 standard deviation, thin-plate smoothing splines    NA       TRUE      10
#>                                                   layer_url
#> 1  https://www.lifewatch.be/sdmpredictors/WC_alt_lonlat.tif
#> 2 https://www.lifewatch.be/sdmpredictors/WC_bio1_lonlat.tif
#> 3 https://www.lifewatch.be/sdmpredictors/WC_bio2_lonlat.tif
#> 4 https://www.lifewatch.be/sdmpredictors/WC_bio3_lonlat.tif
#> 5 https://www.lifewatch.be/sdmpredictors/WC_bio4_lonlat.tif
# list the layercodes all monthly layers from WorldClim
worldclim <- list_layers("WorldClim")
worldclim[!is.na(worldclim$month),]$layer_code
#>  [1] "WC_prec1"   "WC_prec2"   "WC_prec3"   "WC_prec4"   "WC_prec5"  
#>  [6] "WC_prec6"   "WC_prec7"   "WC_prec8"   "WC_prec9"   "WC_prec10" 
#> [11] "WC_prec11"  "WC_prec12"  "WC_tmax1"   "WC_tmax2"   "WC_tmax3"  
#> [16] "WC_tmax4"   "WC_tmax5"   "WC_tmax6"   "WC_tmax7"   "WC_tmax8"  
#> [21] "WC_tmax9"   "WC_tmax10"  "WC_tmax11"  "WC_tmax12"  "WC_tmean1" 
#> [26] "WC_tmean2"  "WC_tmean3"  "WC_tmean4"  "WC_tmean5"  "WC_tmean6" 
#> [31] "WC_tmean7"  "WC_tmean8"  "WC_tmean9"  "WC_tmean10" "WC_tmean11"
#> [36] "WC_tmean12" "WC_tmin1"   "WC_tmin2"   "WC_tmin3"   "WC_tmin4"  
#> [41] "WC_tmin5"   "WC_tmin6"   "WC_tmin7"   "WC_tmin8"   "WC_tmin9"  
#> [46] "WC_tmin10"  "WC_tmin11"  "WC_tmin12" 
# list layer codes for Bio-ORACLE and MARSPEC
list_layers(c("Bio-ORACLE","MARSPEC"))$layer_code
#>   [1] "MS_bathy_5m"                      "MS_biogeo01_aspect_EW_5m"        
#>   [3] "MS_biogeo02_aspect_NS_5m"         "MS_biogeo03_plan_curvature_5m"   
#>   [5] "MS_biogeo04_profile_curvature_5m" "BO_cloudmax"                     
#>   [7] "MS_biogeo05_dist_shore_5m"        "MS_biogeo06_bathy_slope_5m"      
#>   [9] "MS_biogeo07_concavity_5m"         "MS_biogeo08_sss_mean_5m"         
#>  [11] "MS_biogeo09_sss_min_5m"           "MS_biogeo10_sss_max_5m"          
#>  [13] "MS_biogeo11_sss_range_5m"         "MS_biogeo12_sss_variance_5m"     
#>  [15] "MS_biogeo13_sst_mean_5m"          "MS_biogeo14_sst_min_5m"          
#>  [17] "MS_biogeo15_sst_max_5m"           "MS_biogeo16_sst_range_5m"        
#>  [19] "MS_biogeo17_sst_variance_5m"      "MS_sss01_5m"                     
#>  [21] "MS_sss02_5m"                      "MS_sss03_5m"                     
#>  [23] "MS_sss04_5m"                      "MS_sss05_5m"                     
#>  [25] "MS_sss06_5m"                      "MS_sss07_5m"                     
#>  [27] "MS_sss08_5m"                      "MS_sss09_5m"                     
#>  [29] "MS_sss10_5m"                      "MS_sss11_5m"                     
#>  [31] "MS_sss12_5m"                      "MS_sst01_5m"                     
#>  [33] "MS_sst02_5m"                      "MS_sst03_5m"                     
#>  [35] "MS_sst04_5m"                      "MS_sst05_5m"                     
#>  [37] "MS_sst06_5m"                      "MS_sst07_5m"                     
#>  [39] "MS_sst08_5m"                      "MS_sst09_5m"                     
#>  [41] "MS_sst10_5m"                      "MS_sst11_5m"                     
#>  [43] "MS_sst12_5m"                      "BO22_calcite"                    
#>  [45] "BO22_cloudmax"                    "BO22_cloudmean"                  
#>  [47] "BO22_cloudmin"                    "BO22_damax"                      
#>  [49] "BO22_damean"                      "BO22_damin"                      
#>  [51] "BO22_parmax"                      "BO22_parmean"                    
#>  [53] "BO22_ph"                          "BO_calcite"                      
#>  [55] "BO_chlomax"                       "BO_chlomean"                     
#>  [57] "BO_chlomin"                       "BO_chlorange"                    
#>  [59] "BO_cloudmean"                     "BO_cloudmin"                     
#>  [61] "BO_damax"                         "BO_damean"                       
#>  [63] "BO_damin"                         "BO_dissox"                       
#>  [65] "BO_nitrate"                       "BO_parmax"                       
#>  [67] "BO_parmean"                       "BO_ph"                           
#>  [69] "BO_phosphate"                     "BO_salinity"                     
#>  [71] "BO_silicate"                      "BO_sstmax"                       
#>  [73] "BO_sstmean"                       "BO_sstmin"                       
#>  [75] "BO_sstrange"                      "BO_bathymin"                     
#>  [77] "BO_bathymax"                      "BO_bathymean"                    
#>  [79] "BO2_chlomax_bdmax"                "BO2_chlomax_bdmean"              
#>  [81] "BO2_chlomax_bdmin"                "BO2_chlomean_bdmax"              
#>  [83] "BO2_chlomean_bdmean"              "BO2_chlomean_bdmin"              
#>  [85] "BO2_chlomin_bdmax"                "BO2_chlomin_bdmean"              
#>  [87] "BO2_chlomin_bdmin"                "BO2_chlorange_bdmax"             
#>  [89] "BO2_chlorange_bdmean"             "BO2_chlorange_bdmin"             
#>  [91] "BO2_chloltmax_bdmax"              "BO2_chloltmax_bdmean"            
#>  [93] "BO2_chloltmax_bdmin"              "BO2_chloltmin_bdmax"             
#>  [95] "BO2_chloltmin_bdmean"             "BO2_chloltmin_bdmin"             
#>  [97] "BO2_curvelmax_bdmax"              "BO2_curvelmax_bdmean"            
#>  [99] "BO2_curvelmax_bdmin"              "BO2_curvelmean_bdmax"            
#> [101] "BO2_curvelmean_bdmean"            "BO2_curvelmean_bdmin"            
#> [103] "BO2_curvelmin_bdmax"              "BO2_curvelmin_bdmean"            
#> [105] "BO2_curvelmin_bdmin"              "BO2_curvelrange_bdmax"           
#> [107] "BO2_curvelrange_bdmean"           "BO2_curvelrange_bdmin"           
#> [109] "BO2_curvelltmax_bdmax"            "BO2_curvelltmax_bdmean"          
#> [111] "BO2_curvelltmax_bdmin"            "BO2_curvelltmin_bdmax"           
#> [113] "BO2_curvelltmin_bdmean"           "BO2_curvelltmin_bdmin"           
#> [115] "BO2_dissoxmax_bdmax"              "BO2_dissoxmax_bdmean"            
#> [117] "BO2_dissoxmax_bdmin"              "BO2_dissoxmean_bdmax"            
#> [119] "BO2_dissoxmean_bdmean"            "BO2_dissoxmean_bdmin"            
#> [121] "BO2_dissoxmin_bdmax"              "BO2_dissoxmin_bdmean"            
#> [123] "BO2_dissoxmin_bdmin"              "BO2_dissoxrange_bdmax"           
#> [125] "BO2_dissoxrange_bdmean"           "BO2_dissoxrange_bdmin"           
#> [127] "BO2_dissoxltmax_bdmax"            "BO2_dissoxltmax_bdmean"          
#> [129] "BO2_dissoxltmax_bdmin"            "BO2_dissoxltmin_bdmax"           
#> [131] "BO2_dissoxltmin_bdmean"           "BO2_dissoxltmin_bdmin"           
#> [133] "BO2_ironmax_bdmax"                "BO2_ironmax_bdmean"              
#> [135] "BO2_ironmax_bdmin"                "BO2_ironmean_bdmax"              
#> [137] "BO2_ironmean_bdmean"              "BO2_ironmean_bdmin"              
#> [139] "BO2_ironmin_bdmax"                "BO2_ironmin_bdmean"              
#> [141] "BO2_ironmin_bdmin"                "BO2_ironrange_bdmax"             
#> [143] "BO2_ironrange_bdmean"             "BO2_ironrange_bdmin"             
#> [145] "BO2_ironltmax_bdmax"              "BO2_ironltmax_bdmean"            
#> [147] "BO2_ironltmax_bdmin"              "BO2_ironltmin_bdmax"             
#> [149] "BO2_ironltmin_bdmean"             "BO2_ironltmin_bdmin"             
#> [151] "BO2_phosphatemax_bdmax"           "BO2_phosphatemax_bdmean"         
#> [153] "BO2_phosphatemax_bdmin"           "BO2_phosphatemean_bdmax"         
#> [155] "BO2_phosphatemean_bdmean"         "BO2_phosphatemean_bdmin"         
#> [157] "BO2_phosphatemin_bdmax"           "BO2_phosphatemin_bdmean"         
#> [159] "BO2_phosphatemin_bdmin"           "BO2_phosphaterange_bdmax"        
#> [161] "BO2_phosphaterange_bdmean"        "BO2_phosphaterange_bdmin"        
#> [163] "BO2_phosphateltmax_bdmax"         "BO2_phosphateltmax_bdmean"       
#> [165] "BO2_phosphateltmax_bdmin"         "BO2_phosphateltmin_bdmax"        
#> [167] "BO2_phosphateltmin_bdmean"        "BO2_phosphateltmin_bdmin"        
#> [169] "BO2_lightbotmax_bdmax"            "BO2_lightbotmax_bdmean"          
#> [171] "BO2_lightbotmax_bdmin"            "BO2_lightbotmean_bdmax"          
#> [173] "BO2_lightbotmean_bdmean"          "BO2_lightbotmean_bdmin"          
#> [175] "BO2_lightbotmin_bdmax"            "BO2_lightbotmin_bdmean"          
#> [177] "BO2_lightbotmin_bdmin"            "BO2_lightbotrange_bdmax"         
#> [179] "BO2_lightbotrange_bdmean"         "BO2_lightbotrange_bdmin"         
#> [181] "BO2_lightbotltmax_bdmax"          "BO2_lightbotltmax_bdmean"        
#> [183] "BO2_lightbotltmax_bdmin"          "BO2_lightbotltmin_bdmax"         
#> [185] "BO2_lightbotltmin_bdmean"         "BO2_lightbotltmin_bdmin"         
#> [187] "BO2_nitratemax_bdmax"             "BO2_nitratemax_bdmean"           
#> [189] "BO2_nitratemax_bdmin"             "BO2_nitratemean_bdmax"           
#> [191] "BO2_nitratemean_bdmean"           "BO2_nitratemean_bdmin"           
#> [193] "BO2_nitratemin_bdmax"             "BO2_nitratemin_bdmean"           
#> [195] "BO2_nitratemin_bdmin"             "BO2_nitraterange_bdmax"          
#> [197] "BO2_nitraterange_bdmean"          "BO2_nitraterange_bdmin"          
#> [199] "BO2_nitrateltmax_bdmax"           "BO2_nitrateltmax_bdmean"         
#> [201] "BO2_nitrateltmax_bdmin"           "BO2_nitrateltmin_bdmax"          
#> [203] "BO2_nitrateltmin_bdmean"          "BO2_nitrateltmin_bdmin"          
#> [205] "BO2_tempmax_bdmax"                "BO2_tempmax_bdmean"              
#> [207] "BO2_tempmax_bdmin"                "BO2_tempmean_bdmax"              
#> [209] "BO2_tempmean_bdmean"              "BO2_tempmean_bdmin"              
#> [211] "BO2_tempmin_bdmax"                "BO2_tempmin_bdmean"              
#> [213] "BO2_tempmin_bdmin"                "BO2_temprange_bdmax"             
#> [215] "BO2_temprange_bdmean"             "BO2_temprange_bdmin"             
#> [217] "BO2_templtmax_bdmax"              "BO2_templtmax_bdmean"            
#> [219] "BO2_templtmax_bdmin"              "BO2_templtmin_bdmax"             
#> [221] "BO2_templtmin_bdmean"             "BO2_templtmin_bdmin"             
#> [223] "BO2_carbonphytomax_bdmax"         "BO2_carbonphytomax_bdmean"       
#> [225] "BO2_carbonphytomax_bdmin"         "BO2_carbonphytomean_bdmax"       
#> [227] "BO2_carbonphytomean_bdmean"       "BO2_carbonphytomean_bdmin"       
#> [229] "BO2_carbonphytomin_bdmax"         "BO2_carbonphytomin_bdmean"       
#> [231] "BO2_carbonphytomin_bdmin"         "BO2_carbonphytorange_bdmax"      
#> [233] "BO2_carbonphytorange_bdmean"      "BO2_carbonphytorange_bdmin"      
#> [235] "BO2_carbonphytoltmax_bdmax"       "BO2_carbonphytoltmax_bdmean"     
#> [237] "BO2_carbonphytoltmax_bdmin"       "BO2_carbonphytoltmin_bdmax"      
#> [239] "BO2_carbonphytoltmin_bdmean"      "BO2_carbonphytoltmin_bdmin"      
#> [241] "BO2_ppmax_bdmax"                  "BO2_ppmax_bdmean"                
#> [243] "BO2_ppmax_bdmin"                  "BO2_ppmean_bdmax"                
#> [245] "BO2_ppmean_bdmean"                "BO2_ppmean_bdmin"                
#> [247] "BO2_ppmin_bdmax"                  "BO2_ppmin_bdmean"                
#> [249] "BO2_ppmin_bdmin"                  "BO2_pprange_bdmax"               
#> [251] "BO2_pprange_bdmean"               "BO2_pprange_bdmin"               
#> [253] "BO2_ppltmax_bdmax"                "BO2_ppltmax_bdmean"              
#> [255] "BO2_ppltmax_bdmin"                "BO2_ppltmin_bdmax"               
#> [257] "BO2_ppltmin_bdmean"               "BO2_ppltmin_bdmin"               
#> [259] "BO2_salinitymax_bdmax"            "BO2_salinitymax_bdmean"          
#> [261] "BO2_salinitymax_bdmin"            "BO2_salinitymean_bdmax"          
#> [263] "BO2_salinitymean_bdmean"          "BO2_salinitymean_bdmin"          
#> [265] "BO2_salinitymin_bdmax"            "BO2_salinitymin_bdmean"          
#> [267] "BO2_salinitymin_bdmin"            "BO2_salinityrange_bdmax"         
#> [269] "BO2_salinityrange_bdmean"         "BO2_salinityrange_bdmin"         
#> [271] "BO2_salinityltmax_bdmax"          "BO2_salinityltmax_bdmean"        
#> [273] "BO2_salinityltmax_bdmin"          "BO2_salinityltmin_bdmax"         
#> [275] "BO2_salinityltmin_bdmean"         "BO2_salinityltmin_bdmin"         
#> [277] "BO2_silicatemax_bdmax"            "BO2_silicatemax_bdmean"          
#> [279] "BO2_silicatemax_bdmin"            "BO2_silicatemean_bdmax"          
#> [281] "BO2_silicatemean_bdmean"          "BO2_silicatemean_bdmin"          
#> [283] "BO2_silicatemin_bdmax"            "BO2_silicatemin_bdmean"          
#> [285] "BO2_silicatemin_bdmin"            "BO2_silicaterange_bdmax"         
#> [287] "BO2_silicaterange_bdmean"         "BO2_silicaterange_bdmin"         
#> [289] "BO2_silicateltmax_bdmax"          "BO2_silicateltmax_bdmean"        
#> [291] "BO2_silicateltmax_bdmin"          "BO2_silicateltmin_bdmax"         
#> [293] "BO2_silicateltmin_bdmean"         "BO2_silicateltmin_bdmin"         
#> [295] "BO2_icecoverltmax_ss"             "BO2_icecoverltmin_ss"            
#> [297] "BO2_icecovermax_ss"               "BO2_icecovermean_ss"             
#> [299] "BO2_icecovermin_ss"               "BO2_icecoverrange_ss"            
#> [301] "BO2_icethickltmax_ss"             "BO2_icethickltmin_ss"            
#> [303] "BO2_icethickmax_ss"               "BO2_icethickmean_ss"             
#> [305] "BO2_icethickmin_ss"               "BO2_icethickrange_ss"            
#> [307] "BO2_templtmax_ss"                 "BO2_templtmin_ss"                
#> [309] "BO2_tempmax_ss"                   "BO2_tempmean_ss"                 
#> [311] "BO2_tempmin_ss"                   "BO2_temprange_ss"                
#> [313] "BO2_chlomax_ss"                   "BO2_chlomean_ss"                 
#> [315] "BO2_chlomin_ss"                   "BO2_chlorange_ss"                
#> [317] "BO2_chloltmax_ss"                 "BO2_chloltmin_ss"                
#> [319] "BO2_curvelmax_ss"                 "BO2_curvelmean_ss"               
#> [321] "BO2_curvelmin_ss"                 "BO2_curvelrange_ss"              
#> [323] "BO2_curvelltmax_ss"               "BO2_curvelltmin_ss"              
#> [325] "BO2_dissoxmax_ss"                 "BO2_dissoxmean_ss"               
#> [327] "BO2_dissoxmin_ss"                 "BO2_dissoxrange_ss"              
#> [329] "BO2_dissoxltmax_ss"               "BO2_dissoxltmin_ss"              
#> [331] "BO2_ironmax_ss"                   "BO2_ironmean_ss"                 
#> [333] "BO2_ironmin_ss"                   "BO2_ironrange_ss"                
#> [335] "BO2_ironltmax_ss"                 "BO2_ironltmin_ss"                
#> [337] "BO2_phosphatemax_ss"              "BO2_phosphatemean_ss"            
#> [339] "BO2_phosphatemin_ss"              "BO2_phosphaterange_ss"           
#> [341] "BO2_phosphateltmax_ss"            "BO2_phosphateltmin_ss"           
#> [343] "BO2_nitratemax_ss"                "BO2_nitratemean_ss"              
#> [345] "BO2_nitratemin_ss"                "BO2_nitraterange_ss"             
#> [347] "BO2_nitrateltmax_ss"              "BO2_nitrateltmin_ss"             
#> [349] "BO2_carbonphytomax_ss"            "BO2_carbonphytomean_ss"          
#> [351] "BO2_carbonphytomin_ss"            "BO2_carbonphytorange_ss"         
#> [353] "BO2_carbonphytoltmax_ss"          "BO2_carbonphytoltmin_ss"         
#> [355] "BO2_ppmax_ss"                     "BO2_ppmean_ss"                   
#> [357] "BO2_ppmin_ss"                     "BO2_pprange_ss"                  
#> [359] "BO2_ppltmax_ss"                   "BO2_ppltmin_ss"                  
#> [361] "BO2_salinitymax_ss"               "BO2_salinitymean_ss"             
#> [363] "BO2_salinitymin_ss"               "BO2_salinityrange_ss"            
#> [365] "BO2_salinityltmax_ss"             "BO2_salinityltmin_ss"            
#> [367] "BO2_silicatemax_ss"               "BO2_silicatemean_ss"             
#> [369] "BO2_silicatemin_ss"               "BO2_silicaterange_ss"            
#> [371] "BO2_silicateltmax_ss"             "BO2_silicateltmin_ss"            
#> [373] "BO21_carbonphytoltmax_bdmax"      "BO21_carbonphytoltmax_bdmean"    
#> [375] "BO21_carbonphytoltmax_bdmin"      "BO21_carbonphytoltmax_ss"        
#> [377] "BO21_carbonphytoltmin_bdmax"      "BO21_carbonphytoltmin_bdmean"    
#> [379] "BO21_carbonphytoltmin_bdmin"      "BO21_carbonphytoltmin_ss"        
#> [381] "BO21_carbonphytomax_bdmax"        "BO21_carbonphytomax_bdmean"      
#> [383] "BO21_carbonphytomax_bdmin"        "BO21_carbonphytomax_ss"          
#> [385] "BO21_carbonphytomean_bdmax"       "BO21_carbonphytomean_bdmean"     
#> [387] "BO21_carbonphytomean_bdmin"       "BO21_carbonphytomean_ss"         
#> [389] "BO21_carbonphytomin_bdmax"        "BO21_carbonphytomin_bdmean"      
#> [391] "BO21_carbonphytomin_bdmin"        "BO21_carbonphytomin_ss"          
#> [393] "BO21_carbonphytorange_bdmax"      "BO21_carbonphytorange_bdmean"    
#> [395] "BO21_carbonphytorange_bdmin"      "BO21_carbonphytorange_ss"        
#> [397] "BO21_chloltmax_bdmax"             "BO21_chloltmax_bdmean"           
#> [399] "BO21_chloltmax_bdmin"             "BO21_chloltmax_ss"               
#> [401] "BO21_chloltmin_bdmax"             "BO21_chloltmin_bdmean"           
#> [403] "BO21_chloltmin_bdmin"             "BO21_chloltmin_ss"               
#> [405] "BO21_chlomax_bdmax"               "BO21_chlomax_bdmean"             
#> [407] "BO21_chlomax_bdmin"               "BO21_chlomax_ss"                 
#> [409] "BO21_chlomean_bdmax"              "BO21_chlomean_bdmean"            
#> [411] "BO21_chlomean_bdmin"              "BO21_chlomean_ss"                
#> [413] "BO21_chlomin_bdmax"               "BO21_chlomin_bdmean"             
#> [415] "BO21_chlomin_bdmin"               "BO21_chlomin_ss"                 
#> [417] "BO21_chlorange_bdmax"             "BO21_chlorange_bdmean"           
#> [419] "BO21_chlorange_bdmin"             "BO21_chlorange_ss"               
#> [421] "BO21_curvelltmax_bdmax"           "BO21_curvelltmax_bdmean"         
#> [423] "BO21_curvelltmax_bdmin"           "BO21_curvelltmax_ss"             
#> [425] "BO21_curvelltmin_bdmax"           "BO21_curvelltmin_bdmean"         
#> [427] "BO21_curvelltmin_bdmin"           "BO21_curvelltmin_ss"             
#> [429] "BO21_curvelmax_bdmax"             "BO21_curvelmax_bdmean"           
#> [431] "BO21_curvelmax_bdmin"             "BO21_curvelmax_ss"               
#> [433] "BO21_curvelmean_bdmax"            "BO21_curvelmean_bdmean"          
#> [435] "BO21_curvelmean_bdmin"            "BO21_curvelmean_ss"              
#> [437] "BO21_curvelmin_bdmax"             "BO21_curvelmin_bdmean"           
#> [439] "BO21_curvelmin_bdmin"             "BO21_curvelmin_ss"               
#> [441] "BO21_curvelrange_bdmax"           "BO21_curvelrange_bdmean"         
#> [443] "BO21_curvelrange_bdmin"           "BO21_curvelrange_ss"             
#> [445] "BO21_dissoxltmax_bdmax"           "BO21_dissoxltmax_bdmean"         
#> [447] "BO21_dissoxltmax_bdmin"           "BO21_dissoxltmax_ss"             
#> [449] "BO21_dissoxltmin_bdmax"           "BO21_dissoxltmin_bdmean"         
#> [451] "BO21_dissoxltmin_bdmin"           "BO21_dissoxltmin_ss"             
#> [453] "BO21_dissoxmax_bdmax"             "BO21_dissoxmax_bdmean"           
#> [455] "BO21_dissoxmax_bdmin"             "BO21_dissoxmax_ss"               
#> [457] "BO21_dissoxmean_bdmax"            "BO21_dissoxmean_bdmean"          
#> [459] "BO21_dissoxmean_bdmin"            "BO21_dissoxmean_ss"              
#> [461] "BO21_dissoxmin_bdmax"             "BO21_dissoxmin_bdmean"           
#> [463] "BO21_dissoxmin_bdmin"             "BO21_dissoxmin_ss"               
#> [465] "BO21_dissoxrange_bdmax"           "BO21_dissoxrange_bdmean"         
#> [467] "BO21_dissoxrange_bdmin"           "BO21_dissoxrange_ss"             
#> [469] "BO21_icecoverltmax_ss"            "BO21_icecoverltmin_ss"           
#> [471] "BO21_icecovermax_ss"              "BO21_icecovermean_ss"            
#> [473] "BO21_icecovermin_ss"              "BO21_icecoverrange_ss"           
#> [475] "BO21_icethickltmax_ss"            "BO21_icethickltmin_ss"           
#> [477] "BO21_icethickmax_ss"              "BO21_icethickmean_ss"            
#> [479] "BO21_icethickmin_ss"              "BO21_icethickrange_ss"           
#> [481] "BO21_ironltmax_bdmax"             "BO21_ironltmax_bdmean"           
#> [483] "BO21_ironltmax_bdmin"             "BO21_ironltmax_ss"               
#> [485] "BO21_ironltmin_bdmax"             "BO21_ironltmin_bdmean"           
#> [487] "BO21_ironltmin_bdmin"             "BO21_ironltmin_ss"               
#> [489] "BO21_ironmax_bdmax"               "BO21_ironmax_bdmean"             
#> [491] "BO21_ironmax_bdmin"               "BO21_ironmax_ss"                 
#> [493] "BO21_ironmean_bdmax"              "BO21_ironmean_bdmean"            
#> [495] "BO21_ironmean_bdmin"              "BO21_ironmean_ss"                
#> [497] "BO21_ironmin_bdmax"               "BO21_ironmin_bdmean"             
#> [499] "BO21_ironmin_bdmin"               "BO21_ironmin_ss"                 
#> [501] "BO21_ironrange_bdmax"             "BO21_ironrange_bdmean"           
#> [503] "BO21_ironrange_bdmin"             "BO21_ironrange_ss"               
#> [505] "BO21_lightbotltmax_bdmax"         "BO21_lightbotltmax_bdmean"       
#> [507] "BO21_lightbotltmax_bdmin"         "BO21_lightbotltmin_bdmax"        
#> [509] "BO21_lightbotltmin_bdmean"        "BO21_lightbotltmin_bdmin"        
#> [511] "BO21_lightbotmax_bdmax"           "BO21_lightbotmax_bdmean"         
#> [513] "BO21_lightbotmax_bdmin"           "BO21_lightbotmean_bdmax"         
#> [515] "BO21_lightbotmean_bdmean"         "BO21_lightbotmean_bdmin"         
#> [517] "BO21_lightbotmin_bdmax"           "BO21_lightbotmin_bdmean"         
#> [519] "BO21_lightbotmin_bdmin"           "BO21_lightbotrange_bdmax"        
#> [521] "BO21_lightbotrange_bdmean"        "BO21_lightbotrange_bdmin"        
#> [523] "BO21_nitrateltmax_bdmax"          "BO21_nitrateltmax_bdmean"        
#> [525] "BO21_nitrateltmax_bdmin"          "BO21_nitrateltmax_ss"            
#> [527] "BO21_nitrateltmin_bdmax"          "BO21_nitrateltmin_bdmean"        
#> [529] "BO21_nitrateltmin_bdmin"          "BO21_nitrateltmin_ss"            
#> [531] "BO21_nitratemax_bdmax"            "BO21_nitratemax_bdmean"          
#> [533] "BO21_nitratemax_bdmin"            "BO21_nitratemax_ss"              
#> [535] "BO21_nitratemean_bdmax"           "BO21_nitratemean_bdmean"         
#> [537] "BO21_nitratemean_bdmin"           "BO21_nitratemean_ss"             
#> [539] "BO21_nitratemin_bdmax"            "BO21_nitratemin_bdmean"          
#> [541] "BO21_nitratemin_bdmin"            "BO21_nitratemin_ss"              
#> [543] "BO21_nitraterange_bdmax"          "BO21_nitraterange_bdmean"        
#> [545] "BO21_nitraterange_bdmin"          "BO21_nitraterange_ss"            
#> [547] "BO21_phosphateltmax_bdmax"        "BO21_phosphateltmax_bdmean"      
#> [549] "BO21_phosphateltmax_bdmin"        "BO21_phosphateltmax_ss"          
#> [551] "BO21_phosphateltmin_bdmax"        "BO21_phosphateltmin_bdmean"      
#> [553] "BO21_phosphateltmin_bdmin"        "BO21_phosphateltmin_ss"          
#> [555] "BO21_phosphatemax_bdmax"          "BO21_phosphatemax_bdmean"        
#> [557] "BO21_phosphatemax_bdmin"          "BO21_phosphatemax_ss"            
#> [559] "BO21_phosphatemean_bdmax"         "BO21_phosphatemean_bdmean"       
#> [561] "BO21_phosphatemean_bdmin"         "BO21_phosphatemean_ss"           
#> [563] "BO21_phosphatemin_bdmax"          "BO21_phosphatemin_bdmean"        
#> [565] "BO21_phosphatemin_bdmin"          "BO21_phosphatemin_ss"            
#> [567] "BO21_phosphaterange_bdmax"        "BO21_phosphaterange_bdmean"      
#> [569] "BO21_phosphaterange_bdmin"        "BO21_phosphaterange_ss"          
#> [571] "BO21_ppltmax_bdmax"               "BO21_ppltmax_bdmean"             
#> [573] "BO21_ppltmax_bdmin"               "BO21_ppltmax_ss"                 
#> [575] "BO21_ppltmin_bdmax"               "BO21_ppltmin_bdmean"             
#> [577] "BO21_ppltmin_bdmin"               "BO21_ppltmin_ss"                 
#> [579] "BO21_ppmax_bdmax"                 "BO21_ppmax_bdmean"               
#> [581] "BO21_ppmax_bdmin"                 "BO21_ppmax_ss"                   
#> [583] "BO21_ppmean_bdmax"                "BO21_ppmean_bdmean"              
#> [585] "BO21_ppmean_bdmin"                "BO21_ppmean_ss"                  
#> [587] "BO21_ppmin_bdmax"                 "BO21_ppmin_bdmean"               
#> [589] "BO21_ppmin_bdmin"                 "BO21_ppmin_ss"                   
#> [591] "BO21_pprange_bdmax"               "BO21_pprange_bdmean"             
#> [593] "BO21_pprange_bdmin"               "BO21_pprange_ss"                 
#> [595] "BO21_salinityltmax_bdmax"         "BO21_salinityltmax_bdmean"       
#> [597] "BO21_salinityltmax_bdmin"         "BO21_salinityltmax_ss"           
#> [599] "BO21_salinityltmin_bdmax"         "BO21_salinityltmin_bdmean"       
#> [601] "BO21_salinityltmin_bdmin"         "BO21_salinityltmin_ss"           
#> [603] "BO21_salinitymax_bdmax"           "BO21_salinitymax_bdmean"         
#> [605] "BO21_salinitymax_bdmin"           "BO21_salinitymax_ss"             
#> [607] "BO21_salinitymean_bdmax"          "BO21_salinitymean_bdmean"        
#> [609] "BO21_salinitymean_bdmin"          "BO21_salinitymean_ss"            
#> [611] "BO21_salinitymin_bdmax"           "BO21_salinitymin_bdmean"         
#> [613] "BO21_salinitymin_bdmin"           "BO21_salinitymin_ss"             
#> [615] "BO21_salinityrange_bdmax"         "BO21_salinityrange_bdmean"       
#> [617] "BO21_salinityrange_bdmin"         "BO21_salinityrange_ss"           
#> [619] "BO21_silicateltmax_bdmax"         "BO21_silicateltmax_bdmean"       
#> [621] "BO21_silicateltmax_bdmin"         "BO21_silicateltmax_ss"           
#> [623] "BO21_silicateltmin_bdmax"         "BO21_silicateltmin_bdmean"       
#> [625] "BO21_silicateltmin_bdmin"         "BO21_silicateltmin_ss"           
#> [627] "BO21_silicatemax_bdmax"           "BO21_silicatemax_bdmean"         
#> [629] "BO21_silicatemax_bdmin"           "BO21_silicatemax_ss"             
#> [631] "BO21_silicatemean_bdmax"          "BO21_silicatemean_bdmean"        
#> [633] "BO21_silicatemean_bdmin"          "BO21_silicatemean_ss"            
#> [635] "BO21_silicatemin_bdmax"           "BO21_silicatemin_bdmean"         
#> [637] "BO21_silicatemin_bdmin"           "BO21_silicatemin_ss"             
#> [639] "BO21_silicaterange_bdmax"         "BO21_silicaterange_bdmean"       
#> [641] "BO21_silicaterange_bdmin"         "BO21_silicaterange_ss"           
#> [643] "BO21_templtmax_bdmax"             "BO21_templtmax_bdmean"           
#> [645] "BO21_templtmax_bdmin"             "BO21_templtmax_ss"               
#> [647] "BO21_templtmin_bdmax"             "BO21_templtmin_bdmean"           
#> [649] "BO21_templtmin_bdmin"             "BO21_templtmin_ss"               
#> [651] "BO21_tempmax_bdmax"               "BO21_tempmax_bdmean"             
#> [653] "BO21_tempmax_bdmin"               "BO21_tempmax_ss"                 
#> [655] "BO21_tempmean_bdmax"              "BO21_tempmean_bdmean"            
#> [657] "BO21_tempmean_bdmin"              "BO21_tempmean_ss"                
#> [659] "BO21_tempmin_bdmax"               "BO21_tempmin_bdmean"             
#> [661] "BO21_tempmin_bdmin"               "BO21_tempmin_ss"                 
#> [663] "BO21_temprange_bdmax"             "BO21_temprange_bdmean"           
#> [665] "BO21_temprange_bdmin"             "BO21_temprange_ss"               
#> [667] "BO22_carbonphytoltmax_bdmax"      "BO22_carbonphytoltmax_bdmean"    
#> [669] "BO22_carbonphytoltmax_bdmin"      "BO22_carbonphytoltmax_ss"        
#> [671] "BO22_carbonphytoltmin_bdmax"      "BO22_carbonphytoltmin_bdmean"    
#> [673] "BO22_carbonphytoltmin_bdmin"      "BO22_carbonphytoltmin_ss"        
#> [675] "BO22_carbonphytomax_bdmax"        "BO22_carbonphytomax_bdmean"      
#> [677] "BO22_carbonphytomax_bdmin"        "BO22_carbonphytomax_ss"          
#> [679] "BO22_carbonphytomean_bdmax"       "BO22_carbonphytomean_bdmean"     
#> [681] "BO22_carbonphytomean_bdmin"       "BO22_carbonphytomean_ss"         
#> [683] "BO22_carbonphytomin_bdmax"        "BO22_carbonphytomin_bdmean"      
#> [685] "BO22_carbonphytomin_bdmin"        "BO22_carbonphytomin_ss"          
#> [687] "BO22_carbonphytorange_bdmax"      "BO22_carbonphytorange_bdmean"    
#> [689] "BO22_carbonphytorange_bdmin"      "BO22_carbonphytorange_ss"        
#> [691] "BO22_chloltmax_bdmax"             "BO22_chloltmax_bdmean"           
#> [693] "BO22_chloltmax_bdmin"             "BO22_chloltmax_ss"               
#> [695] "BO22_chloltmin_bdmax"             "BO22_chloltmin_bdmean"           
#> [697] "BO22_chloltmin_bdmin"             "BO22_chloltmin_ss"               
#> [699] "BO22_chlomax_bdmax"               "BO22_chlomax_bdmean"             
#> [701] "BO22_chlomax_bdmin"               "BO22_chlomax_ss"                 
#> [703] "BO22_chlomean_bdmax"              "BO22_chlomean_bdmean"            
#> [705] "BO22_chlomean_bdmin"              "BO22_chlomean_ss"                
#> [707] "BO22_chlomin_bdmax"               "BO22_chlomin_bdmean"             
#> [709] "BO22_chlomin_bdmin"               "BO22_chlomin_ss"                 
#> [711] "BO22_chlorange_bdmax"             "BO22_chlorange_bdmean"           
#> [713] "BO22_chlorange_bdmin"             "BO22_chlorange_ss"               
#> [715] "BO22_curvelltmax_bdmax"           "BO22_curvelltmax_bdmean"         
#> [717] "BO22_curvelltmax_bdmin"           "BO22_curvelltmax_ss"             
#> [719] "BO22_curvelltmin_bdmax"           "BO22_curvelltmin_bdmean"         
#> [721] "BO22_curvelltmin_bdmin"           "BO22_curvelltmin_ss"             
#> [723] "BO22_curvelmax_bdmax"             "BO22_curvelmax_bdmean"           
#> [725] "BO22_curvelmax_bdmin"             "BO22_curvelmax_ss"               
#> [727] "BO22_curvelmean_bdmax"            "BO22_curvelmean_bdmean"          
#> [729] "BO22_curvelmean_bdmin"            "BO22_curvelmean_ss"              
#> [731] "BO22_curvelmin_bdmax"             "BO22_curvelmin_bdmean"           
#> [733] "BO22_curvelmin_bdmin"             "BO22_curvelmin_ss"               
#> [735] "BO22_curvelrange_bdmax"           "BO22_curvelrange_bdmean"         
#> [737] "BO22_curvelrange_bdmin"           "BO22_curvelrange_ss"             
#> [739] "BO22_dissoxltmax_bdmax"           "BO22_dissoxltmax_bdmean"         
#> [741] "BO22_dissoxltmax_bdmin"           "BO22_dissoxltmax_ss"             
#> [743] "BO22_dissoxltmin_bdmax"           "BO22_dissoxltmin_bdmean"         
#> [745] "BO22_dissoxltmin_bdmin"           "BO22_dissoxltmin_ss"             
#> [747] "BO22_dissoxmax_bdmax"             "BO22_dissoxmax_bdmean"           
#> [749] "BO22_dissoxmax_bdmin"             "BO22_dissoxmax_ss"               
#> [751] "BO22_dissoxmean_bdmax"            "BO22_dissoxmean_bdmean"          
#> [753] "BO22_dissoxmean_bdmin"            "BO22_dissoxmean_ss"              
#> [755] "BO22_dissoxmin_bdmax"             "BO22_dissoxmin_bdmean"           
#> [757] "BO22_dissoxmin_bdmin"             "BO22_dissoxmin_ss"               
#> [759] "BO22_dissoxrange_bdmax"           "BO22_dissoxrange_bdmean"         
#> [761] "BO22_dissoxrange_bdmin"           "BO22_dissoxrange_ss"             
#> [763] "BO22_icecoverltmax_ss"            "BO22_icecoverltmin_ss"           
#> [765] "BO22_icecovermax_ss"              "BO22_icecovermean_ss"            
#> [767] "BO22_icecovermin_ss"              "BO22_icecoverrange_ss"           
#> [769] "BO22_icethickltmax_ss"            "BO22_icethickltmin_ss"           
#> [771] "BO22_icethickmax_ss"              "BO22_icethickmean_ss"            
#> [773] "BO22_icethickmin_ss"              "BO22_icethickrange_ss"           
#> [775] "BO22_ironltmax_bdmax"             "BO22_ironltmax_bdmean"           
#> [777] "BO22_ironltmax_bdmin"             "BO22_ironltmax_ss"               
#> [779] "BO22_ironltmin_bdmax"             "BO22_ironltmin_bdmean"           
#> [781] "BO22_ironltmin_bdmin"             "BO22_ironltmin_ss"               
#> [783] "BO22_ironmax_bdmax"               "BO22_ironmax_bdmean"             
#> [785] "BO22_ironmax_bdmin"               "BO22_ironmax_ss"                 
#> [787] "BO22_ironmean_bdmax"              "BO22_ironmean_bdmean"            
#> [789] "BO22_ironmean_bdmin"              "BO22_ironmean_ss"                
#> [791] "BO22_ironmin_bdmax"               "BO22_ironmin_bdmean"             
#> [793] "BO22_ironmin_bdmin"               "BO22_ironmin_ss"                 
#> [795] "BO22_ironrange_bdmax"             "BO22_ironrange_bdmean"           
#> [797] "BO22_ironrange_bdmin"             "BO22_ironrange_ss"               
#> [799] "BO22_lightbotltmax_bdmax"         "BO22_lightbotltmax_bdmean"       
#> [801] "BO22_lightbotltmax_bdmin"         "BO22_lightbotltmin_bdmax"        
#> [803] "BO22_lightbotltmin_bdmean"        "BO22_lightbotltmin_bdmin"        
#> [805] "BO22_lightbotmax_bdmax"           "BO22_lightbotmax_bdmean"         
#> [807] "BO22_lightbotmax_bdmin"           "BO22_lightbotmean_bdmax"         
#> [809] "BO22_lightbotmean_bdmean"         "BO22_lightbotmean_bdmin"         
#> [811] "BO22_lightbotmin_bdmax"           "BO22_lightbotmin_bdmean"         
#> [813] "BO22_lightbotmin_bdmin"           "BO22_lightbotrange_bdmax"        
#> [815] "BO22_lightbotrange_bdmean"        "BO22_lightbotrange_bdmin"        
#> [817] "BO22_nitrateltmax_bdmax"          "BO22_nitrateltmax_bdmean"        
#> [819] "BO22_nitrateltmax_bdmin"          "BO22_nitrateltmax_ss"            
#> [821] "BO22_nitrateltmin_bdmax"          "BO22_nitrateltmin_bdmean"        
#> [823] "BO22_nitrateltmin_bdmin"          "BO22_nitrateltmin_ss"            
#> [825] "BO22_nitratemax_bdmax"            "BO22_nitratemax_bdmean"          
#> [827] "BO22_nitratemax_bdmin"            "BO22_nitratemax_ss"              
#> [829] "BO22_nitratemean_bdmax"           "BO22_nitratemean_bdmean"         
#> [831] "BO22_nitratemean_bdmin"           "BO22_nitratemean_ss"             
#> [833] "BO22_nitratemin_bdmax"            "BO22_nitratemin_bdmean"          
#> [835] "BO22_nitratemin_bdmin"            "BO22_nitratemin_ss"              
#> [837] "BO22_nitraterange_bdmax"          "BO22_nitraterange_bdmean"        
#> [839] "BO22_nitraterange_bdmin"          "BO22_nitraterange_ss"            
#> [841] "BO22_phosphateltmax_bdmax"        "BO22_phosphateltmax_bdmean"      
#> [843] "BO22_phosphateltmax_bdmin"        "BO22_phosphateltmax_ss"          
#> [845] "BO22_phosphateltmin_bdmax"        "BO22_phosphateltmin_bdmean"      
#> [847] "BO22_phosphateltmin_bdmin"        "BO22_phosphateltmin_ss"          
#> [849] "BO22_phosphatemax_bdmax"          "BO22_phosphatemax_bdmean"        
#> [851] "BO22_phosphatemax_bdmin"          "BO22_phosphatemax_ss"            
#> [853] "BO22_phosphatemean_bdmax"         "BO22_phosphatemean_bdmean"       
#> [855] "BO22_phosphatemean_bdmin"         "BO22_phosphatemean_ss"           
#> [857] "BO22_phosphatemin_bdmax"          "BO22_phosphatemin_bdmean"        
#> [859] "BO22_phosphatemin_bdmin"          "BO22_phosphatemin_ss"            
#> [861] "BO22_phosphaterange_bdmax"        "BO22_phosphaterange_bdmean"      
#> [863] "BO22_phosphaterange_bdmin"        "BO22_phosphaterange_ss"          
#> [865] "BO22_ppltmax_bdmax"               "BO22_ppltmax_bdmean"             
#> [867] "BO22_ppltmax_bdmin"               "BO22_ppltmax_ss"                 
#> [869] "BO22_ppltmin_bdmax"               "BO22_ppltmin_bdmean"             
#> [871] "BO22_ppltmin_bdmin"               "BO22_ppltmin_ss"                 
#> [873] "BO22_ppmax_bdmax"                 "BO22_ppmax_bdmean"               
#> [875] "BO22_ppmax_bdmin"                 "BO22_ppmax_ss"                   
#> [877] "BO22_ppmean_bdmax"                "BO22_ppmean_bdmean"              
#> [879] "BO22_ppmean_bdmin"                "BO22_ppmean_ss"                  
#> [881] "BO22_ppmin_bdmax"                 "BO22_ppmin_bdmean"               
#> [883] "BO22_ppmin_bdmin"                 "BO22_ppmin_ss"                   
#> [885] "BO22_pprange_bdmax"               "BO22_pprange_bdmean"             
#> [887] "BO22_pprange_bdmin"               "BO22_pprange_ss"                 
#> [889] "BO22_salinityltmax_bdmax"         "BO22_salinityltmax_bdmean"       
#> [891] "BO22_salinityltmax_bdmin"         "BO22_salinityltmax_ss"           
#> [893] "BO22_salinityltmin_bdmax"         "BO22_salinityltmin_bdmean"       
#> [895] "BO22_salinityltmin_bdmin"         "BO22_salinityltmin_ss"           
#> [897] "BO22_salinitymax_bdmax"           "BO22_salinitymax_bdmean"         
#> [899] "BO22_salinitymax_bdmin"           "BO22_salinitymax_ss"             
#> [901] "BO22_salinitymean_bdmax"          "BO22_salinitymean_bdmean"        
#> [903] "BO22_salinitymean_bdmin"          "BO22_salinitymean_ss"            
#> [905] "BO22_salinitymin_bdmax"           "BO22_salinitymin_bdmean"         
#> [907] "BO22_salinitymin_bdmin"           "BO22_salinitymin_ss"             
#> [909] "BO22_salinityrange_bdmax"         "BO22_salinityrange_bdmean"       
#> [911] "BO22_salinityrange_bdmin"         "BO22_salinityrange_ss"           
#> [913] "BO22_silicateltmax_bdmax"         "BO22_silicateltmax_bdmean"       
#> [915] "BO22_silicateltmax_bdmin"         "BO22_silicateltmax_ss"           
#> [917] "BO22_silicateltmin_bdmax"         "BO22_silicateltmin_bdmean"       
#> [919] "BO22_silicateltmin_bdmin"         "BO22_silicateltmin_ss"           
#> [921] "BO22_silicatemax_bdmax"           "BO22_silicatemax_bdmean"         
#> [923] "BO22_silicatemax_bdmin"           "BO22_silicatemax_ss"             
#> [925] "BO22_silicatemean_bdmax"          "BO22_silicatemean_bdmean"        
#> [927] "BO22_silicatemean_bdmin"          "BO22_silicatemean_ss"            
#> [929] "BO22_silicatemin_bdmax"           "BO22_silicatemin_bdmean"         
#> [931] "BO22_silicatemin_bdmin"           "BO22_silicatemin_ss"             
#> [933] "BO22_silicaterange_bdmax"         "BO22_silicaterange_bdmean"       
#> [935] "BO22_silicaterange_bdmin"         "BO22_silicaterange_ss"           
#> [937] "BO22_templtmax_bdmax"             "BO22_templtmax_bdmean"           
#> [939] "BO22_templtmax_bdmin"             "BO22_templtmax_ss"               
#> [941] "BO22_templtmin_bdmax"             "BO22_templtmin_bdmean"           
#> [943] "BO22_templtmin_bdmin"             "BO22_templtmin_ss"               
#> [945] "BO22_tempmax_bdmax"               "BO22_tempmax_bdmean"             
#> [947] "BO22_tempmax_bdmin"               "BO22_tempmax_ss"                 
#> [949] "BO22_tempmean_bdmax"              "BO22_tempmean_bdmean"            
#> [951] "BO22_tempmean_bdmin"              "BO22_tempmean_ss"                
#> [953] "BO22_tempmin_bdmax"               "BO22_tempmin_bdmean"             
#> [955] "BO22_tempmin_bdmin"               "BO22_tempmin_ss"                 
#> [957] "BO22_temprange_bdmax"             "BO22_temprange_bdmean"           
#> [959] "BO22_temprange_bdmin"             "BO22_temprange_ss"               
# list the first 5 terrestrial layers
list_layers(marine=FALSE)[1:5,]
#>   dataset_code layer_code                           name
#> 1    WorldClim     WC_alt                       Altitude
#> 2    WorldClim    WC_bio1        Annual mean temperature
#> 3    WorldClim    WC_bio2 Mean diurnal temperature range
#> 4    WorldClim    WC_bio3                  Isothermality
#> 5    WorldClim    WC_bio4        Temperature seasonality
#>                                                               description
#> 1                                                                Altitude
#> 2                                                 Annual mean temperature
#> 3         Mean of the monthly (maximum temperature - minimum temperature)
#> 4 Mean diurnal temperature range (bio2) / Annual temperature range (bio7)
#> 5                       Standard deviation of the annual mean temperature
#>   terrestrial marine freshwater cellsize_equalarea cellsize_lonlat   units
#> 1        TRUE  FALSE      FALSE               7000      0.08333333  meters
#> 2        TRUE  FALSE      FALSE               7000      0.08333333 Celsius
#> 3        TRUE  FALSE      FALSE               7000      0.08333333 Celsius
#> 4        TRUE  FALSE      FALSE               7000      0.08333333 Celsius
#> 5        TRUE  FALSE      FALSE               7000      0.08333333 Celsius
#>                primary_type primary_spatial_resolution
#> 1 Satellite (SRTM), GTOPO30   3 arcsecond, 30 arsecond
#> 2       in situ measurement                         ''
#> 3       in situ measurement                         ''
#> 4       in situ measurement                         ''
#> 5       in situ measurement                         ''
#>                                                                                                                                                                                                                                           primary_source
#> 1  SRTM: CGIAR-CSI, reference: Jarvis, A., H.I. Reuter, A. Nelson, E. Guevara, 2008, Hole-filled SRTM for the globe Version 4, available from the CGIAR-CSI SRTM 90m Database (http://srtm.csi.cgiar.org). GTOPO30: url: https://lta.cr.usgs.gov/GTOPO30
#> 2 Weather station data from multiple sources, see Hijmans, R.J., S.E. Cameron, J.L. Parra, P.G. Jones and A. Jarvis, 2005. Very high resolution interpolated climate surfaces for global land areas. International Journal of Climatology 25: 1965-1978.
#> 3 Weather station data from multiple sources, see Hijmans, R.J., S.E. Cameron, J.L. Parra, P.G. Jones and A. Jarvis, 2005. Very high resolution interpolated climate surfaces for global land areas. International Journal of Climatology 25: 1965-1978.
#> 4 Weather station data from multiple sources, see Hijmans, R.J., S.E. Cameron, J.L. Parra, P.G. Jones and A. Jarvis, 2005. Very high resolution interpolated climate surfaces for global land areas. International Journal of Climatology 25: 1965-1978.
#> 5 Weather station data from multiple sources, see Hijmans, R.J., S.E. Cameron, J.L. Parra, P.G. Jones and A. Jarvis, 2005. Very high resolution interpolated climate surfaces for global land areas. International Journal of Climatology 25: 1965-1978.
#>   start_year start_month start_day end_year end_month end_day
#> 1       1950           1         1     2000         1       1
#> 2       1950           1         1     2000         1       1
#> 3       1950           1         1     2000         1       1
#> 4       1950           1         1     2000         1       1
#> 5       1950           1         1     2000         1       1
#>                                         derivation month is_surface version
#> 1                                               ''    NA       TRUE      10
#> 2               mean, thin-plate smoothing splines    NA       TRUE      10
#> 3         mean range, thin-plate smoothing splines    NA       TRUE      10
#> 4                                               ''    NA       TRUE      10
#> 5 standard deviation, thin-plate smoothing splines    NA       TRUE      10
#>                                                   layer_url
#> 1  https://www.lifewatch.be/sdmpredictors/WC_alt_lonlat.tif
#> 2 https://www.lifewatch.be/sdmpredictors/WC_bio1_lonlat.tif
#> 3 https://www.lifewatch.be/sdmpredictors/WC_bio2_lonlat.tif
#> 4 https://www.lifewatch.be/sdmpredictors/WC_bio3_lonlat.tif
#> 5 https://www.lifewatch.be/sdmpredictors/WC_bio4_lonlat.tif
# list the first 5 marine layers
list_layers(terrestrial=FALSE)[1:5,]
#>    dataset_code                       layer_code               name
#> 69      MARSPEC                      MS_bathy_5m         Bathymetry
#> 70      MARSPEC         MS_biogeo01_aspect_EW_5m   East/West aspect
#> 71      MARSPEC         MS_biogeo02_aspect_NS_5m North/South Aspect
#> 72      MARSPEC    MS_biogeo03_plan_curvature_5m     Plan curvature
#> 73      MARSPEC MS_biogeo04_profile_curvature_5m  Profile curvature
#>                                                                                                                                                                                          description
#> 69                                                                                                                                                                             Depth of the seafloor
#> 70                                                                                                                                                         East/West Aspect (sin(aspect in radians))
#> 71                                                                                                                                                       North/South Aspect (cos(aspect in radians))
#> 72    Plan curvature is the curvature in the direction perpendicular to the maximum slope and indicates whether flow across a surface would diverge (positive values) or converge (negative values).
#> 73 Profile curvature is the curvature in the direction parallel to the maximum slope and indicates whether flow across a surface would accelerate (positive values) or decelerate (negative values).
#>    terrestrial marine freshwater cellsize_equalarea cellsize_lonlat    units
#> 69       FALSE   TRUE      FALSE               7000      0.08333333   meters
#> 70       FALSE   TRUE      FALSE               7000      0.08333333  radians
#> 71       FALSE   TRUE      FALSE               7000      0.08333333  radians
#> 72       FALSE   TRUE      FALSE               7000      0.08333333 unitless
#> 73       FALSE   TRUE      FALSE               7000      0.08333333 unitless
#>        primary_type primary_spatial_resolution
#> 69 Satellite (SRTM)               30 arcsecond
#> 70 Satellite (SRTM)               30 arcsecond
#> 71 Satellite (SRTM)               30 arcsecond
#> 72 Satellite (SRTM)               30 arcsecond
#> 73 Satellite (SRTM)               30 arcsecond
#>                                                                                         primary_source
#> 69 SRTM30_PLUS V6.0 reference: Becker et al. 2009 URL: http://topex.ucsd.edu/WWW_html/srtm30_plus.html
#> 70 SRTM30_PLUS V6.0 reference: Becker et al. 2009 URL: http://topex.ucsd.edu/WWW_html/srtm30_plus.html
#> 71 SRTM30_PLUS V6.0 reference: Becker et al. 2009 URL: http://topex.ucsd.edu/WWW_html/srtm30_plus.html
#> 72 SRTM30_PLUS V6.0 reference: Becker et al. 2009 URL: http://topex.ucsd.edu/WWW_html/srtm30_plus.html
#> 73 SRTM30_PLUS V6.0 reference: Becker et al. 2009 URL: http://topex.ucsd.edu/WWW_html/srtm30_plus.html
#>    start_year start_month start_day end_year end_month end_day
#> 69       2009           1         1     2009        12      31
#> 70       2009           1         1     2009        12      31
#> 71       2009           1         1     2009        12      31
#> 72       2009           1         1     2009        12      31
#> 73       2009           1         1     2009        12      31
#>                                          derivation month is_surface version
#> 69                                               ''    NA       TRUE      10
#> 70 derived from bathymetry (sin(aspect in radians))    NA       TRUE      10
#> 71 derived from bathymetry (cos(aspect in radians))    NA       TRUE      10
#> 72                           derived from bathmetry    NA       TRUE      10
#> 73                           derived from bathmetry    NA       TRUE      10
#>                                                                             layer_url
#> 69                      https://www.lifewatch.be/sdmpredictors/MS_bathy_5m_lonlat.tif
#> 70         https://www.lifewatch.be/sdmpredictors/MS_biogeo01_aspect_EW_5m_lonlat.tif
#> 71         https://www.lifewatch.be/sdmpredictors/MS_biogeo02_aspect_NS_5m_lonlat.tif
#> 72    https://www.lifewatch.be/sdmpredictors/MS_biogeo03_plan_curvature_5m_lonlat.tif
#> 73 https://www.lifewatch.be/sdmpredictors/MS_biogeo04_profile_curvature_5m_lonlat.tif
# list all annual MARSPEC layers (remove monthly layers)
list_layers("MARSPEC", monthly = FALSE)
#>    dataset_code                       layer_code
#> 69      MARSPEC                      MS_bathy_5m
#> 70      MARSPEC         MS_biogeo01_aspect_EW_5m
#> 71      MARSPEC         MS_biogeo02_aspect_NS_5m
#> 72      MARSPEC    MS_biogeo03_plan_curvature_5m
#> 73      MARSPEC MS_biogeo04_profile_curvature_5m
#> 75      MARSPEC        MS_biogeo05_dist_shore_5m
#> 76      MARSPEC       MS_biogeo06_bathy_slope_5m
#> 77      MARSPEC         MS_biogeo07_concavity_5m
#> 78      MARSPEC          MS_biogeo08_sss_mean_5m
#> 79      MARSPEC           MS_biogeo09_sss_min_5m
#> 80      MARSPEC           MS_biogeo10_sss_max_5m
#> 81      MARSPEC         MS_biogeo11_sss_range_5m
#> 82      MARSPEC      MS_biogeo12_sss_variance_5m
#> 83      MARSPEC          MS_biogeo13_sst_mean_5m
#> 84      MARSPEC           MS_biogeo14_sst_min_5m
#> 85      MARSPEC           MS_biogeo15_sst_max_5m
#> 86      MARSPEC         MS_biogeo16_sst_range_5m
#> 87      MARSPEC      MS_biogeo17_sst_variance_5m
#>                                                name
#> 69                                       Bathymetry
#> 70                                 East/West aspect
#> 71                               North/South Aspect
#> 72                                   Plan curvature
#> 73                                Profile curvature
#> 75                                Distance to shore
#> 76                                Bathymetric slope
#> 77                                        Concavity
#> 78               Sea surface salinity (annual mean)
#> 79           Sea surface salinity (monthly minimum)
#> 80           Sea surface salinity (monthly maximum)
#> 81              Sea surface salinity (annual range)
#> 82           Sea surface salinity (annual variance)
#> 83            Sea surface temperature (annual mean)
#> 84 Sea surface temperature (coldest ice-free month)
#> 85 Sea surface temperature (warmest ice-free month)
#> 86                  Sea surface temperature (range)
#> 87               Sea surface temperature (variance)
#>                                                                                                                                                                                                                                                                             description
#> 69                                                                                                                                                                                                                                                                Depth of the seafloor
#> 70                                                                                                                                                                                                                                            East/West Aspect (sin(aspect in radians))
#> 71                                                                                                                                                                                                                                          North/South Aspect (cos(aspect in radians))
#> 72                                                                                       Plan curvature is the curvature in the direction perpendicular to the maximum slope and indicates whether flow across a surface would diverge (positive values) or converge (negative values).
#> 73                                                                                    Profile curvature is the curvature in the direction parallel to the maximum slope and indicates whether flow across a surface would accelerate (positive values) or decelerate (negative values).
#> 75                                                                                                                                                                                                                                                                                   ''
#> 76                                                                                                                                                                                Bathymetric slope was measured in degrees ranging from 0??? (flat surface) to 90??? (vertical slope).
#> 77                                                                                         Concavity is the second derivative of the bathymetry layer (or the slope of the slope) and represents whether a raster cell is on a hill (negative values) or in a valley (positive values).
#> 78                                                                                                             Measurements of sea surface salinity (SSS) were obtained from in situ oceanographic observations compiled by NOAA?s World Ocean Atlas 2009 (WOA09, Antonov et al. 2010).
#> 79                                                                                                             Measurements of sea surface salinity (SSS) were obtained from in situ oceanographic observations compiled by NOAA?s World Ocean Atlas 2009 (WOA09, Antonov et al. 2010).
#> 80                                                                                                             Measurements of sea surface salinity (SSS) were obtained from in situ oceanographic observations compiled by NOAA?s World Ocean Atlas 2009 (WOA09, Antonov et al. 2010).
#> 81                                                                                                             Measurements of sea surface salinity (SSS) were obtained from in situ oceanographic observations compiled by NOAA?s World Ocean Atlas 2009 (WOA09, Antonov et al. 2010).
#> 82                                                                                                             Measurements of sea surface salinity (SSS) were obtained from in situ oceanographic observations compiled by NOAA?s World Ocean Atlas 2009 (WOA09, Antonov et al. 2010).
#> 83 Satellite measures of sea surface temperature (SST) were obtained at a 2.5 arc-minute resolution (approximately 4 km_) from Aqua-MODIS 4-micron nighttime SST Level 3 standard mapped image products, downloaded from NASA's Ocean Color website (http://oceancolor.gsfc.nasa.gov/).
#> 84 Satellite measures of sea surface temperature (SST) were obtained at a 2.5 arc-minute resolution (approximately 4 km_) from Aqua-MODIS 4-micron nighttime SST Level 3 standard mapped image products, downloaded from NASA's Ocean Color website (http://oceancolor.gsfc.nasa.gov/).
#> 85 Satellite measures of sea surface temperature (SST) were obtained at a 2.5 arc-minute resolution (approximately 4 km_) from Aqua-MODIS 4-micron nighttime SST Level 3 standard mapped image products, downloaded from NASA's Ocean Color website (http://oceancolor.gsfc.nasa.gov/).
#> 86 Satellite measures of sea surface temperature (SST) were obtained at a 2.5 arc-minute resolution (approximately 4 km_) from Aqua-MODIS 4-micron nighttime SST Level 3 standard mapped image products, downloaded from NASA's Ocean Color website (http://oceancolor.gsfc.nasa.gov/).
#> 87 Satellite measures of sea surface temperature (SST) were obtained at a 2.5 arc-minute resolution (approximately 4 km_) from Aqua-MODIS 4-micron nighttime SST Level 3 standard mapped image products, downloaded from NASA's Ocean Color website (http://oceancolor.gsfc.nasa.gov/).
#>    terrestrial marine freshwater cellsize_equalarea cellsize_lonlat      units
#> 69       FALSE   TRUE      FALSE               7000      0.08333333     meters
#> 70       FALSE   TRUE      FALSE               7000      0.08333333    radians
#> 71       FALSE   TRUE      FALSE               7000      0.08333333    radians
#> 72       FALSE   TRUE      FALSE               7000      0.08333333   unitless
#> 73       FALSE   TRUE      FALSE               7000      0.08333333   unitless
#> 75       FALSE   TRUE      FALSE               7000      0.08333333 kilometers
#> 76       FALSE   TRUE      FALSE               7000      0.08333333    degrees
#> 77       FALSE   TRUE      FALSE               7000      0.08333333    degrees
#> 78       FALSE   TRUE      FALSE               7000      0.08333333        psu
#> 79       FALSE   TRUE      FALSE               7000      0.08333333        psu
#> 80       FALSE   TRUE      FALSE               7000      0.08333333        psu
#> 81       FALSE   TRUE      FALSE               7000      0.08333333        psu
#> 82       FALSE   TRUE      FALSE               7000      0.08333333        psu
#> 83       FALSE   TRUE      FALSE               7000      0.08333333    Celsius
#> 84       FALSE   TRUE      FALSE               7000      0.08333333    Celsius
#> 85       FALSE   TRUE      FALSE               7000      0.08333333    Celsius
#> 86       FALSE   TRUE      FALSE               7000      0.08333333    Celsius
#> 87       FALSE   TRUE      FALSE               7000      0.08333333    Celsius
#>                                     primary_type primary_spatial_resolution
#> 69                              Satellite (SRTM)               30 arcsecond
#> 70                              Satellite (SRTM)               30 arcsecond
#> 71                              Satellite (SRTM)               30 arcsecond
#> 72                              Satellite (SRTM)               30 arcsecond
#> 73                              Satellite (SRTM)               30 arcsecond
#> 75                                            ''                         ''
#> 76                              Satellite (SRTM)               30 arcsecond
#> 77                              Satellite (SRTM)               30 arcsecond
#> 78   in situ measurements, monthly climatologies                1 arcdegree
#> 79   in situ measurements, monthly climatologies                1 arcdegree
#> 80   in situ measurements, monthly climatologies                1 arcdegree
#> 81   in situ measurements, monthly climatologies                1 arcdegree
#> 82   in situ measurements, monthly climatologies                1 arcdegree
#> 83 Satellite (Aqua-MODIS), monthly climatologies              2.5 arcminute
#> 84 Satellite (Aqua-MODIS), monthly climatologies              2.5 arcminute
#> 85 Satellite (Aqua-MODIS), monthly climatologies              2.5 arcminute
#> 86 Satellite (Aqua-MODIS), monthly climatologies              2.5 arcminute
#> 87 Satellite (Aqua-MODIS), monthly climatologies              2.5 arcminute
#>                                                                                         primary_source
#> 69 SRTM30_PLUS V6.0 reference: Becker et al. 2009 URL: http://topex.ucsd.edu/WWW_html/srtm30_plus.html
#> 70 SRTM30_PLUS V6.0 reference: Becker et al. 2009 URL: http://topex.ucsd.edu/WWW_html/srtm30_plus.html
#> 71 SRTM30_PLUS V6.0 reference: Becker et al. 2009 URL: http://topex.ucsd.edu/WWW_html/srtm30_plus.html
#> 72 SRTM30_PLUS V6.0 reference: Becker et al. 2009 URL: http://topex.ucsd.edu/WWW_html/srtm30_plus.html
#> 73 SRTM30_PLUS V6.0 reference: Becker et al. 2009 URL: http://topex.ucsd.edu/WWW_html/srtm30_plus.html
#> 75   GSHHS v2.1 reference:Wessel and Smith 1996 URL:http://www.ngdc.noaa.gov/mgg/shorelines/gshhs.html
#> 76 SRTM30_PLUS V6.0 reference: Becker et al. 2009 URL: http://topex.ucsd.edu/WWW_html/srtm30_plus.html
#> 77 SRTM30_PLUS V6.0 reference: Becker et al. 2009 URL: http://topex.ucsd.edu/WWW_html/srtm30_plus.html
#> 78                                                World Ocean Atlas 2009 reference:Antonov et al. 2010
#> 79                                                World Ocean Atlas 2009 reference:Antonov et al. 2010
#> 80                                                World Ocean Atlas 2009 reference:Antonov et al. 2010
#> 81                                                World Ocean Atlas 2009 reference:Antonov et al. 2010
#> 82                                                World Ocean Atlas 2009 reference:Antonov et al. 2010
#> 83                           Reference: (Feldman & McClain 2010) URL: http://oceancolor.gsfc.nasa.gov/
#> 84                           Reference: (Feldman & McClain 2010) URL: http://oceancolor.gsfc.nasa.gov/
#> 85                           Reference: (Feldman & McClain 2010) URL: http://oceancolor.gsfc.nasa.gov/
#> 86                           Reference: (Feldman & McClain 2010) URL: http://oceancolor.gsfc.nasa.gov/
#> 87                           Reference: (Feldman & McClain 2010) URL: http://oceancolor.gsfc.nasa.gov/
#>    start_year start_month start_day end_year end_month end_day
#> 69       2009           1         1     2009        12      31
#> 70       2009           1         1     2009        12      31
#> 71       2009           1         1     2009        12      31
#> 72       2009           1         1     2009        12      31
#> 73       2009           1         1     2009        12      31
#> 75       2009           1         1     2009        12      31
#> 76       2009           1         1     2009        12      31
#> 77       2009           1         1     2009        12      31
#> 78       1955           1         1     2006        12      31
#> 79       1955           1         1     2006        12      31
#> 80       1955           1         1     2006        12      31
#> 81       1955           1         1     2006        12      31
#> 82       1955           1         1     2006        12      31
#> 83       2002           1         1     2010        12      31
#> 84       2002           1         1     2010        12      31
#> 85       2002           1         1     2010        12      31
#> 86       2002           1         1     2010        12      31
#> 87       2002           1         1     2010        12      31
#>                                          derivation month is_surface version
#> 69                                               ''    NA       TRUE      10
#> 70 derived from bathymetry (sin(aspect in radians))    NA       TRUE      10
#> 71 derived from bathymetry (cos(aspect in radians))    NA       TRUE      10
#> 72                           derived from bathmetry    NA       TRUE      10
#> 73                           derived from bathmetry    NA       TRUE      10
#> 75                     derived from GSHHS Coastline    NA       TRUE      10
#> 76                           derived from bathmetry    NA       TRUE      10
#> 77                           derived from bathmetry    NA       TRUE      10
#> 78                       mean, spline interpolation    NA       TRUE      10
#> 79                    minimum, spline interpolation    NA       TRUE      10
#> 80                    maximum, spline interpolation    NA       TRUE      10
#> 81                      range, spline interpolation    NA       TRUE      10
#> 82                   variance, spline interpolation    NA       TRUE      10
#> 83                                             mean    NA       TRUE      10
#> 84                                          minimum    NA       TRUE      10
#> 85                                          maximum    NA       TRUE      10
#> 86                                            range    NA       TRUE      10
#> 87                                         variance    NA       TRUE      10
#>                                                                             layer_url
#> 69                      https://www.lifewatch.be/sdmpredictors/MS_bathy_5m_lonlat.tif
#> 70         https://www.lifewatch.be/sdmpredictors/MS_biogeo01_aspect_EW_5m_lonlat.tif
#> 71         https://www.lifewatch.be/sdmpredictors/MS_biogeo02_aspect_NS_5m_lonlat.tif
#> 72    https://www.lifewatch.be/sdmpredictors/MS_biogeo03_plan_curvature_5m_lonlat.tif
#> 73 https://www.lifewatch.be/sdmpredictors/MS_biogeo04_profile_curvature_5m_lonlat.tif
#> 75        https://www.lifewatch.be/sdmpredictors/MS_biogeo05_dist_shore_5m_lonlat.tif
#> 76       https://www.lifewatch.be/sdmpredictors/MS_biogeo06_bathy_slope_5m_lonlat.tif
#> 77         https://www.lifewatch.be/sdmpredictors/MS_biogeo07_concavity_5m_lonlat.tif
#> 78          https://www.lifewatch.be/sdmpredictors/MS_biogeo08_sss_mean_5m_lonlat.tif
#> 79           https://www.lifewatch.be/sdmpredictors/MS_biogeo09_sss_min_5m_lonlat.tif
#> 80           https://www.lifewatch.be/sdmpredictors/MS_biogeo10_sss_max_5m_lonlat.tif
#> 81         https://www.lifewatch.be/sdmpredictors/MS_biogeo11_sss_range_5m_lonlat.tif
#> 82      https://www.lifewatch.be/sdmpredictors/MS_biogeo12_sss_variance_5m_lonlat.tif
#> 83          https://www.lifewatch.be/sdmpredictors/MS_biogeo13_sst_mean_5m_lonlat.tif
#> 84           https://www.lifewatch.be/sdmpredictors/MS_biogeo14_sst_min_5m_lonlat.tif
#> 85           https://www.lifewatch.be/sdmpredictors/MS_biogeo15_sst_max_5m_lonlat.tif
#> 86         https://www.lifewatch.be/sdmpredictors/MS_biogeo16_sst_range_5m_lonlat.tif
#> 87      https://www.lifewatch.be/sdmpredictors/MS_biogeo17_sst_variance_5m_lonlat.tif
```
