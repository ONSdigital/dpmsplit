
<!-- README.md is generated from README.Rmd. Please edit that file -->

# dpmsplit

<!-- badges: start -->

[![R-CMD-check](https://github.com/ONSdigital/dpmsplit/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/ONSDigital/dpmsplit/actions/workflows/R-CMD-check.yaml)

[![Style](https://github.com/ONSdigital/dpmsplit/actions/workflows/style.yaml/badge.svg)](https://github.com/ONSDigital/dpmsplit/actions/workflows/style.yaml)

[![Contributor
Covenant](https://img.shields.io/badge/Contributor%20Covenant-2.1-4baaaa.svg)](code_of_conduct.md)
<!-- badges: end -->

## Overview

Package **dpmsplit** is for taking estimates of total inflows or total
outflows and splitting them out into component flows. For instance,
**dpmsplit** might be used to turn estimates of total moves out of a
region into (i) moves from the region to elsewhere in the country and
(ii) moves from the region to overseas.

## Contributor guidance

**To see the complete [contributor
guidance](https://github.com/ONSdigital/dpmsplit/blob/main/CONTRIBUTING.md)**

**In brief:** Contributors with write access to this repository should
create and make changes on a feature branch. Contributors without write
access should fork this repository to their own GitHub account and make
changes on a new branch.

All modifications to this package must be made through **Pull
requests**, ideally linked to a specific GitHub **Issue**.

Pull requests must be reviewed by someone other than the requester, if
more than one individual was responsible for the requested changes the
reviewer must be someone other than the main contributor, ideally it
will be reviewed by someone entirely uninvolved with the particular
changes made.

Pull requests should have a suitable pull request review form attached,
with key notes/descriptions of changes made added by the requester and
if applicable additional notes on particular areas to focus the review
(syntax changes, key functional changes, documentational changes etc.)

## Getting started

### Installation

#### Install directly from GitHub

If you have linked your RStudio installation to GitHub you should be
able to install the package directly from GitHub using the
install_github method from the devtools package

``` r
# install.packages("devtools")
library(devtools)

devtools::install_github("ONSdigital/dpmsplit", build_vignettes = TRUE, INSTALL_opts = "--no-multiarch")
```

If you have not/are unable to link your RStudio installation to GitHub
(you may encounter a 404 error when attempting the previous approach)
you can also install the package in two alternative ways

#### Install locally from .zip

Download a copy of the package repository as a .zip file (option in
‘Code’, below Open with GitHub Desktop) and install using the
install_local method from the devtools package (replace the path with
the path to the .zip file download location)

``` r
# install.packages("devtools")
library(devtools)

devtools::install_local("C:/.../Downloads/dpmsplit-main.zip", build_vignettes = TRUE, INSTALL_opts = "--no-multiarch")
```

#### Install locally from cloned repository

Clone the repository (options in ‘Code’ to HTTPS/SSH paths) and build
the package using the build() method from the devtools package

``` r
# install.packages("devtools")
library(devtools)

dpmsplit_build <- devtools::build("~/put/the/package/path/here")

devtools::install_local(dpmsplit_build, build_vignettes = TRUE, INSTALL_opts = "--no-multiarch")
```

## Usage (example)

In this example, we demonstrate the main features of `dpmsplit` using dummy datasets for the estimated totals and reported counts of migration. The package can be used to split migration counts for both a single population and multiple populations, so we will start with the process of splitting a single population’s migration counts.

### Single Population

First, we need to load the package.

``` r
library(dpmsplit)
```

Then, we define a data frame for the estimated totals of migration, which would be taken from Step 2 of the DPM. 

``` r
totals <- data.frame(
  age = c("20-24", "25-29", "20-24", "25-29"),
  sex = c("Female", "Female", "Male", "Male"),
  count = c(20, 24, 15, 8)
)

totals
#>     age    sex count
#> 1 20-24 Female    20
#> 2 25-29 Female    24
#> 3 20-24   Male    15
#> 4 25-29   Male     8
```

Next, we define a data frame for the reported counts of migration. 

``` r
reported <- data.frame(
  age = c("20-24", "25-29", "20-24", "25-29"),
  sex = c("Female", "Female", "Male", "Male"),
  internal = c(10, 15, 12, 3),
  cross_border = c(7, 8, 5, 0),
  international = c(3, 5, 1, 4)
)

reported
#>     age    sex internal cross_border international
#> 1 20-24 Female       10            7             3
#> 2 25-29 Female       15            8             5
#> 3 20-24   Male       12            5             1
#> 4 25-29   Male        3            0             4
```

And then, optionally, we define a list of data frames which hold the alterability scores for the reported counts. These scores are used for weighting, so if we know there are some inaccuracies in the input data we can reflect its reliability by increasing the alterability score for that data frame. Otherwise, the default values for alterability are 1, meaning the data will not be altered. 

``` r
alter <- list(
  cross_border = 1.2,
  international = data.frame(
  age = c("20-24", "25-29"),
  alter = c(2, 2.5)
  )
)

alter
#> $cross_border
#> [1] 1.2
#> 
#> $international
#>     age alter
#> 1 20-24   2.0
#> 2 25-29   2.5
```

Now, with all of our data frames defined, we can call the function `split_single` which performs the splitting of migration counts for a single population,

``` r
split_single(
  totals = totals,
  reported = reported,
  alter = alter,
  zeros_to_ones = FALSE
)
```

giving us the following output.

``` r
#>     age    sex  internal cross_border international
#> 1 20-24 Female 10.000000      7.00000      3.000000
#> 2 25-29 Female 13.382749      6.96496      3.652291
#> 3 20-24   Male 10.200000      4.10000      0.700000
#> 4 25-29   Male  3.230769      0.00000      4.769231
```

### Multiple Populations

The process for splitting multiple populations is very similar, and we again start by defining data frames for the total estimated counts of migration, but this time we have two data frames, one for inflows and one for outflows.

``` r
totals_in <- data.frame(
  year = c(2020, 2020, 2021, 2021),
  region = c("a", "b", "a", "b"),
  count = c(13, 8, 5, 12)
)

totals_out <- data.frame(
  year = c(2020, 2020, 2021, 2021),
  region = c("a", "b", "a", "b"),
  count = c(9, 11, 8, 11)
)

totals_in
#>   year region count
#> 1 2020      a    13
#> 2 2020      b     8
#> 3 2021      a     5
#> 4 2021      b    12

totals_out
#>   year region count
#> 1 2020      a     9
#> 2 2020      b    11
#> 3 2021      a     8
#> 4 2021      b    11
```

Next, we define the data frames for the reported counts of migration. For multiple populations we need a separate data frame for each component of migration, those being internal migration, external immigration, and external emigration. 

``` r
reported_int <- data.frame(
  year = c(2020, 2020, 2021, 2021),
  region_orig = c("a", "b", "a", "b"),
  region_dest = c("b", "a", "b", "a"),
  count = c(3, 8, 4, 2)
)

reported_im <- data.frame(
  year = c(2020, 2020, 2021, 2021),
  region_orig = c("x", "x"),
  region_dest = c("a", "b"),
  count = c(4, 1)
)

reported_em <- data.frame(
  year = c(2020, 2020, 2021, 2021),
  region_orig = c("a", "b"),
  region_dest = c("x", "x"),
  count = c(3, 5)
)

reported_int
#>   year region_orig region_dest count
#> 1 2020           a           b     3
#> 2 2020           b           a     8
#> 3 2021           a           b     4
#> 4 2021           b           a     2

reported_im
#>   year region_orig region_dest count
#> 1 2020           x           a     4
#> 2 2020           x           b     1
#> 3 2021           x           a     4
#> 4 2021           x           b     1

reported_em
#>   year region_orig region_dest count
#> 1 2020           a           x     3
#> 2 2020           b           x     5
#> 3 2021           a           x     3
#> 4 2021           b           x     5
```

Finally, we call the function `split_multi` using the inputs we have defined. This function performs the migration splitting process for multiple populations. 

```r
split_multi(
  totals_in = totals_in,
  totals_out = totals_out,
  reported_int = reported_int,
  reported_im = reported_im,
  reported_em = reported_em,
  epsilon = 0.001,
  max_iter = 1000L,
  tolerance = 1e-6
)
```

And this gives us the output as a list of data frames.

``` r
#> $internal
#>   year region_orig region_dest    count
#> 1 2020           a           a 0.000000
#> 2 2020           b           a 7.543107
#> 3 2020           a           b 5.999000
#> 4 2020           b           b 0.000000
#> 5 2021           a           a 0.000000
#> 6 2021           b           a 2.322976
#> 7 2021           a           b 6.962336
#> 8 2021           b           b 0.000000
#> 
#> $immigration
#>   year region_orig region_dest    count
#> 1 2020           x           a 5.456893
#> 2 2020           x           b 2.001000
#> 3 2021           x           a 2.677024
#> 4 2021           x           b 5.037664
#>
#> $emigration
#>   year region_orig region_dest    count
#> 1 2020           a           x 3.001000
#> 2 2020           b           x 3.456893
#> 3 2021           a           x 1.037665
#> 4 2021           b           x 8.677024
```
