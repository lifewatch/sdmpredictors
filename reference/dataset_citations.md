# Generate dataset citations

`dataset_citations` returns dataset citations as text or as
"[`bibentry`](https://rdrr.io/r/utils/bibentry.html)" objects.

## Usage

``` r
dataset_citations(datasets = c(), astext = TRUE)
```

## Arguments

- datasets:

  character vector. Code of the datasets. When no datasets are provided
  (default), then all citations are returned.

- astext:

  logical. When `TRUE` (default), then citations are returned as text
  otherwise they are returned as
  "[`bibentry`](https://rdrr.io/r/utils/bibentry.html)" objects.

## Value

Either a character vector or a list of
"[`bibentry`](https://rdrr.io/r/utils/bibentry.html)" objects.

## Details

Note that in order to generate a full list of citations it is preferable
to run the
[`layer_citations`](http://lifewatch.github.io/sdmpredictors/reference/layer_citations.md)
function.

## See also

[`layer_citations`](http://lifewatch.github.io/sdmpredictors/reference/layer_citations.md),
[`bibentry`](https://rdrr.io/r/utils/bibentry.html),
[`list_datasets`](http://lifewatch.github.io/sdmpredictors/reference/list_datasets.md)

## Examples

``` r
# print the Bio-ORACLE citation
print(dataset_citations("Bio-ORACLE"))
#> [1] "Assis J, Tyberghein L, Bosch S, Heroen V, Serrão E, De Clerck O, Tittensor D (2018). “Bio‐ORACLE v2.0: Extending marine data layers for\nbioclimatic modelling.” _Global Ecology and Biogeography_, *27*(3),\n277-284. doi:10.1111/geb.12693 <https://doi.org/10.1111/geb.12693>."                                 
#> [2] "Tyberghein L, Heroen V, Pauly K, Troupin C, Mineur F, De Clerck O (2012). “Bio-ORACLE: a global environmental dataset for marine species\ndistribution modelling.” _Global Ecology and Biogeography_, *21*(2),\n272-281. doi:10.1111/j.1466-8238.2011.00656.x\n<https://doi.org/10.1111/j.1466-8238.2011.00656.x>."

# print all citations as Bibtex
print(lapply(dataset_citations(astext = FALSE), toBibtex))
#> $WorldClim
#> @Article{WorldClim,
#>   author = {Robert J. Hijmans and Susan E. Cameron and Juan L. Parra and Peter G. Jones and Andy Jarvis},
#>   title = {Very high resolution interpolated climate surfaces for global land areas.},
#>   journal = {International Journal of Climatology},
#>   year = {2005},
#>   volume = {25},
#>   number = {15},
#>   pages = {1965-1978},
#>   doi = {10.1002/joc.1276},
#> }
#> 
#> $`Bio-ORACLE`
#> @Article{Bio-ORACLEv2,
#>   author = {Jorge Assis and Lennert Tyberghein and Samuel Bosch and Verbruggen Heroen and Ester A. Serrão and Olivier {De Clerck} and Derek Tittensor},
#>   title = {Bio‐ORACLE v2.0: Extending marine data layers for bioclimatic modelling},
#>   journal = {Global Ecology and Biogeography},
#>   year = {2018},
#>   volume = {27},
#>   number = {3},
#>   pages = {277-284},
#>   doi = {10.1111/geb.12693},
#> }
#> 
#> @Article{Bio-ORACLE,
#>   author = {Lennert Tyberghein and Verbruggen Heroen and Klaas Pauly and Charles Troupin and Frederic Mineur and Olivier {De Clerck}},
#>   title = {Bio-ORACLE: a global environmental dataset for marine species distribution modelling},
#>   journal = {Global Ecology and Biogeography},
#>   year = {2012},
#>   volume = {21},
#>   number = {2},
#>   pages = {272-281},
#>   doi = {10.1111/j.1466-8238.2011.00656.x},
#> }
#> 
#> $`Bio-ORACLE_Future`
#> @Article{Bio-ORACLE_Future,
#>   author = {Alexander Jueterbock and Lennert Tyberghein and Verbruggen Heroen and James A. Coyer and Jeanine L. Olsen and Galice Hoarau},
#>   title = {Climate change impact on seaweed meadow distribution in the North Atlantic rocky intertidal},
#>   journal = {Ecology and Evolution},
#>   year = {2013},
#>   volume = {3},
#>   number = {5},
#>   pages = {1356-1373},
#>   doi = {10.1002/ece3.541},
#> }
#> 
#> $MARSPEC
#> @Article{MARSPEC,
#>   author = {Elizabeth J. Sbrocco and Paul H. Barber},
#>   title = {MARSPEC: ocean climate layers for marine spatial ecology},
#>   year = {2013},
#>   volume = {94},
#>   number = {4},
#>   pages = {979},
#>   journal = {Ecology},
#>   doi = {10.1890/12-1358.1},
#> }
#> 
#> $`Paleo-MARSPEC`
#> @Article{Paleo-MARSPEC,
#>   author = {Elizabeth J. Sbrocco},
#>   title = {Paleo-MARSPEC: gridded ocean climate layers for the mid-Holocene and Last Glacial Maximum},
#>   journal = {Ecology},
#>   year = {2014},
#>   volume = {95},
#>   number = {6},
#>   pages = {1710},
#>   doi = {10.1890/14-0443.1},
#> }
#> 
#> $ENVIREM
#> @Article{ENVIREM,
#>   author = {Pascal O. Title and Jordan B. Bemmels},
#>   title = {ENVIREM: An expanded set of bioclimatic and topographic variables increases flexibility and improves performance of ecological niche modeling.},
#>   journal = {Ecography},
#>   year = {2017},
#>   doi = {10.1111/ecog.02880},
#> }
#> 
#> $Freshwater
#> @Article{Freshwater,
#>   author = {S. Domisch and G. Amutelli and W. Jetz},
#>   title = {Near-global freshwater-specific environmental variables for biodiversity analyses in 1 km resolution},
#>   journal = {Scientific Data},
#>   year = {2015},
#>   doi = {10.1038/sdata.2015.73},
#> }
#> 
```
