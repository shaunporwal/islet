
<!-- README.md is generated from README.Rmd. Please edit that file -->

# islet

<!-- badges: start -->
<!-- badges: end -->

The goal of islet is to help establish secure data pipelines for data
cleaning and analytics, designed for teams analyzing patient outcome
data. With Islet, you can easily clean and process sensitive patient
data, while generating reports that track data irregularities and
issues.

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

You can also embed plots, for example:

<img src="man/figures/README-pressure-1.png" width="100%" />

In that case, don’t forget to commit and push the resulting figure
files, so they display on GitHub and CRAN.
