
<a href="https://shaunporwal.com" class="nav-link">← Back to Main
Site</a>

<!-- README.md is generated from README.Rmd. Please edit that file -->

# islet

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
