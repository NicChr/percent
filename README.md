
<!-- README.md is generated from README.Rmd. Please edit that file -->

# percent

<!-- badges: start -->

[![R-CMD-check](https://github.com/NicChr/percent/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/NicChr/percent/actions/workflows/R-CMD-check.yaml)

<!-- badges: end -->

A lightweight percent class that allows easy formatting of percentages.

## Installation

You can install the development version of percent from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("NicChr/percent")
```

## Motivation

In day-to-day analyses I always find myself in this general workflow:

- Create one vector of proportions
- Create another vector of formatted percentages
- Make sure to use the proportions for math operations and the
  percentages for pretty outputs

percent aims to reduce this workflow by combining the two vectors into
one, reducing the work needed to manage independent vectors.

## Examples

We can create percentages using `percent()`

``` r
library(percent)
percent(0:10)
#>  [1] "0%"  "1%"  "2%"  "3%"  "4%"  "5%"  "6%"  "7%"  "8%"  "9%"  "10%"
```

This is simply a wrapper that converts 10 to the proportion 0.1 and
prints it nicely as 10%.

### Math operations

In a nutshell this means we can use it for further mathematical
operations.

``` r
100 * percent(50)
#> [1] 50
```

When we do math operations on 2 percent vectors, a percent vector is
returned

``` r
percent(1:10) + percent(20)
#>  [1] "21%" "22%" "23%" "24%" "25%" "26%" "27%" "28%" "29%" "30%"
```

### Proportions

When you have proportions, just convert them to percentages before using
`percent()`

``` r
prop <- seq(0, 1, 0.1)

percent(prop * 100)
#>  [1] "0%"   "10%"  "20%"  "30%"  "40%"  "50%"  "60%"  "70%"  "80%"  "90%" 
#> [11] "100%"
```

To avoid this task of always multiplying by 100, one can just create a
simple wrapper.

The reason this isn’t already implemented is because percent is designed
to be lightweight and has only one function `percent`.

``` r
as_percent <- function(x){
  percent(x * 100)
}
as_percent(0.05)
#> [1] "5%"
```

### Formatting

To round it to a specified number of decimal places, we can just use
`round()`

``` r
p <- percent(15.56)
round(p)
#> [1] "16%"
round(p, digits = 1)
#> [1] "15.6%"
```

The other rounding operators will also work as expected

``` r
p2 <- percent(0.0005)
signif(p2, 2)
#> [1] "5e-04%"
floor(p2)
#> [1] "0%"
ceiling(p2)
#> [1] "1%"
```

We can also format a percent using the `format()` function

``` r
# 2 significant digits
format(percent(2.674), digits = 2, symbol = "(%)")
#> [1] "2.7(%)"
```

A key note to point out is the digits in `format()` are significant and
not decimal. To round using decimal places, use `round()` beforehand.

``` r
format(round(percent(2.674), 2), symbol = "(%)")
#> [1] "2.67(%)"
```
