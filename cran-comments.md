## Test environments
* Windows 10, R 4.0.2
* Ubuntu 16.04.6 LTS (on travis-ci.org), R 4.0.2
* win-builder (devel and release)
 
## R CMD check results

There were no ERRORs or WARNINGs. 

There was a note when checking on https://win-builder.r-project.org

```
* checking CRAN incoming feasibility ... NOTE
Maintainer: 'Salvador Fernandez <salvador.fernandez@vliz.be>'

New submission

Package was archived on CRAN

Possibly mis-spelled words in DESCRIPTION:
  ENVIREM (16:5)
  MARSPEC (17:9)
  WorldClim (15:38)

CRAN repository db overrides:
  X-CRAN-Comment: Archived on 2020-10-05 as check issues were not
    corrected in time.
```

These names are acronyms of projects. They are not misspelled.

This is a re-submission. New layers of Bio-Oracle were made available to the package (the package pulls the layers from https://www.lifewatch.be/sdmpredictors/). In principle there was no need to update the package, but these new layers caused some tests to fail. These issues are solved now.

More info on new version 2.1. of Bio-Oracle: https://bio-oracle.org/release-notes-2-1.php


## Downstream dependencies

There are currently no downstream dependencies for this package.


