
<!-- README.md is generated from README.Rmd. Please edit that file -->

# islet

<!-- badges: start -->

[![R-CMD-check](https://github.com/shaunporwal/islet/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/shaunporwal/islet/actions/workflows/R-CMD-check.yaml)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

Islet contains personal R functions. I’m also using this to learn how to
create R packages.

## Installation

You can install the development version of islet from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("shaunporwal/islet")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(islet)
```

``` r

# Simple Example, Default '&' Border
islet::make_banner(str_to_banner = 'analyze column from dataframe')
#> #&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
#> #&&  analyze column from dataframe  &&
#> #&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
# Output Saved to Clipboard, Can Cmd+v or Ctrl+v to directly paste banner
```

``` r

# New Banner Border Example
islet::make_banner(str_to_banner = 'analyze column from dataframe',
                   banner_chr = '+')
#> #+++++++++++++++++++++++++++++++++++++
#> #++  analyze column from dataframe  ++
#> #+++++++++++++++++++++++++++++++++++++
```

You’ll still need to render `README.Rmd` regularly, to keep `README.md`
up-to-date. `devtools::build_readme()` is handy for this. You could also
use GitHub Actions to re-render `README.Rmd` every time you push. An
example workflow can be found here:
<https://github.com/r-lib/actions/tree/v1/examples>.
