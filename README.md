
<!-- README.md is generated from README.Rmd. Please edit that file -->

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![Travis build
status](https://travis-ci.org/JohnCoene/ericsson.svg?branch=master)](https://travis-ci.org/JohnCoene/ericsson)
<!-- badges: end -->

# ericsson

{ericsson} attempts at integrating [Erlang](https://www.erlang.org/)
with R.

:warning: This is still under heavy development.

## Example

Requires erlang installed.

``` r
library(ericsson)

e <- Erl$new()

e$eval("4 + 3.")
#> [1] "7"
```

Create a `readme` module.

``` erlang
%% readme.erl
-module(readme).
-export([fac/1]).

fac(1) ->
    1;
fac(N) ->
    N * fac(N - 1).
```

Then compile and use the module.

``` r
e$compile("readme")
#> [1] "{ok,readme}"
e$eval("readme:fac(4).")
#> [1] "24"
```

Finally halt the session.

``` r
e$halt()
#> ✔ Erlang session halted.
```

## Get it

Install the package from Github.

``` r
# install.packages("remotes")
remotes::install_github("JohnCoene/ericsson")
```

Install [Erlang](https://www.erlang.org/).

### Mac OS

Use homebrew.

``` bash
brew install erlang
```

### Ubuntu

With the Linux package manager.

``` bash
sudo apt-get install erlang
```

### Windows

Get the executable from the [Erlang
website](https://www.erlang.org/downloads).
