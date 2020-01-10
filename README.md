
<!-- README.md is generated from README.Rmd. Please edit that file -->

# nrc

<!-- badges: start -->

[![Travis-CI Build
Status](https://travis-ci.org/igjit/nrc.svg?branch=master)](https://travis-ci.org/igjit/nrc)
[![Coverage
status](https://codecov.io/gh/igjit/nrc/branch/master/graph/badge.svg)](https://codecov.io/github/igjit/nrc?branch=master)
<!-- badges: end -->

nrc aims to be an R version of [9cc](https://github.com/rui314/9cc): R
compiler written in R.

## Installation

You can install nrc from github with:

``` r
# install.packages("remotes")
remotes::install_github("igjit/nrc")
```

Note: currently nrc only works on Linux.

Try in Docker:

``` sh
$ docker run --rm -it rocker/tidyverse:3.6.2 R -q
> remotes::install_github("igjit/nrc")
```

## How to play

``` r
library(nrc)

compile("1 + 2")
#> .intel_syntax noprefix
#> .global main
#> main:
#>   push rbp
#>   mov rbp, rsp
#>   sub rsp, 0
#>   push 1
#>   push 2
#>   pop rdi
#>   pop rax
#>   add rax, rdi
#>   push rax
#>   pop rax
#>   mov rsp, rbp
#>   pop rbp
#>   ret
compile("1 + 2") %>% assemble() %>% execute()
#> [1] 3
compile("a <- 2; a * 3") %>% assemble() %>% execute()
#> [1] 6
compile("add2 <- function(x) x + 2; add2(40)") %>% assemble() %>% execute()
#> [1] 42
```
