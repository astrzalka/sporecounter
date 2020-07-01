
<!-- README.md is generated from README.Rmd. Please edit that file -->

# sporecounter

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
<!-- badges: end -->

The goal of spocounter is to make Streptomyces defects analysis easier
by using plot profiles generated from ImageJ and R package Peaks. This
way positions of all septa and nuclei can be found, along with distances
between septa and spores lacking DNA signal.

## Installation

You can install the released version of smoother from Github:

``` r
devtools::install_github('astrzalka/sporecounter')

sporecounter::run_app()
```

However package Peaks is no longer available on CRAN and has been moved
to archive. Installation requires building from source which can be
problematic. Package Peaks already built for Windows can be downloaded
from our disk.

sporecounter includes example datasets stored in data-raw directory and
included in the app itself for easy testing.
