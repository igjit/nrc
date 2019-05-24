
<!-- README.md is generated from README.Rmd. Please edit that file -->
nrc
===

<!-- badges: start -->
[![Travis-CI Build Status](https://travis-ci.org/igjit/nrc.svg?branch=master)](https://travis-ci.org/igjit/nrc) [![Coverage status](https://codecov.io/gh/igjit/nrc/branch/master/graph/badge.svg)](https://codecov.io/github/igjit/nrc?branch=master) <!-- badges: end -->

nrc aims to be an R version of [9cc](https://github.com/rui314/9cc): R compiler written in R.

Installation
------------

You can install nrc from github with:

``` r
# install.packages("devtools")
devtools::install_github("igjit/nrc")
```

Note: currently nrc only works on Linux.

Try in Docker:

``` sh
$ docker run --rm -it rocker/tidyverse:3.6.0 R -q
> devtools::install_github("igjit/nrc")
```
