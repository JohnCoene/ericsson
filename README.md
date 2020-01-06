
<!-- README.md is generated from README.Rmd. Please edit that file -->

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
<!-- badges: end -->

# errlang

{errlang} attempts at integrating [Erlang](https://www.erlang.org/) with
R.

:warning: This is still under heavy development.

## Example

Requires erlang installed.

``` r
library(errlang)

e <- Erl$new()

e$eval("4 + 3.")
#> [1] "7"
```

Create a `readme` module and compile it.

``` erlang
%% readme.erl
-module(readme).
-export([fac/1]).

fac(1) ->
    1;
fac(N) ->
    N * fac(N - 1).
```

Then in R.

``` r
e$compile("readme")
#> [1] "{ok,readme}"
e$eval("readme:fac(4).")
#> [1] "24"
```

## Get it

Install it from Github.

``` r
# install.packages("remotes")
remotes::install_github("JohnCoene/errlang")
```
