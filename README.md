
TODO: - fix folde /data. Is needes? (now in buildignore) - Remove
get_anomalies_season, Nacho_workflow_demo.R, data-raw and work_flow.R
(now in buildignore) - tests are failing (both get and evenness) -
examples fail, recheck - Datasets (if any) need documentation. - generar
web con packege down -

<!-- README.md is generated from README.Rmd. Please edit that file -->

# Bloomers

<!-- badges: start -->

[![R-CMD-check](https://github.com/EcologyR/templateRpackage/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/EcologyR/templateRpackage/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/EcologyR/templateRpackage/branch/master/graph/badge.svg)](https://app.codecov.io/gh/EcologyR/templateRpackage?branch=master)
[![](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![Project Status: WIP - Initial development is in progress, but there
has not yet been a stable, usable release suitable for the
public.](https://www.repostatus.org/badges/latest/wip.svg)](https://www.repostatus.org/#wip)
<!-- [![CodeFactor](https://www.codefactor.io/repository/github/ecologyr/templaterpackage/badge)](https://www.codefactor.io/repository/github/ecologyr/templaterpackage) -->
<!-- badges: end -->

The main objective of Bloomers package is to detect sharp changes in
abundances for individual taxa in microbial communities.

## Installation

``` r
# install.packages("devtools")
devtools::install_github("EcologyR/Bloomers")
```

The code to create this package is available
[here](https://github.com/EcologyR/Bloomers).

\##The workflow of the bloomers package is summarized in the following
graph

![](man/figures/shceme_bloomers_pk.png)

## Example

This is a basic example which shows you how to solve a common problem:

``` r
# library(templateRpackage)
## basic example code
```

What is special about using `README.Rmd` instead of just `README.md`?
You can include R chunks like so:

``` r
summary(cars)
#>      speed           dist       
#>  Min.   : 4.0   Min.   :  2.00  
#>  1st Qu.:12.0   1st Qu.: 26.00  
#>  Median :15.0   Median : 36.00  
#>  Mean   :15.4   Mean   : 42.98  
#>  3rd Qu.:19.0   3rd Qu.: 56.00  
#>  Max.   :25.0   Max.   :120.00
```

You’ll still need to render `README.Rmd` regularly, to keep `README.md`
up-to-date. `devtools::build_readme()` is handy for this. You could also
use GitHub Actions to re-render `README.Rmd` every time you push. An
example workflow can be found here:
<https://github.com/r-lib/actions/tree/v1/examples>.

Put here a plot representing blooming species and how we do detect
anomalies

``` r

#plot(bloomersdata$pseudoabundance) #improve this example 
#load("./data/bloomersdata.rda")
```

``` r
plot(pressure)
```

In that case, don’t forget to commit and push the resulting figure
files, so they display on GitHub and CRAN.

## Citation

If using this package, please cite it:

``` r
#citation("Bloomers")
```

## Funding

The development of this software has been funded by Fondo Europeo de
Desarrollo Regional (FEDER) and Consejería de Transformación Económica,
Industria, Conocimiento y Universidades of Junta de Andalucía (proyecto
US-1381388 led by Francisco Rodríguez Sánchez, Universidad de Sevilla).

![](man/figures/ICM-logotip.jpg)
