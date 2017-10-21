<!-- README.md is generated from README.Rmd. Please edit that file -->
[MICE: Multivariate Imputation by Chained Equations](http://stefvanbuuren.github.io/mice/)
==========================================================================================

The [`mice`](https://cran.r-project.org/package=mice) package implements a method to deal with missing data. The package creates multiple imputations (replacement values) for multivariate missing data. The method is based on Fully Conditional Specification, where each incomplete variable is imputed by a separate model. The `MICE` algorithm can impute mixes of continuous, binary, unordered categorical and ordered categorical data. In addition, MICE can impute continuous two-level data, and maintain consistency between imputations by means of passive imputation. Many diagnostic plots are implemented to inspect the quality of the imputations.

Installation
------------

The `mice` package can be installed from CRAN as follows:

``` r
install.packages("mice")
```

The latest version is can be installed from GitHub as follows:

``` r
install.packages("devtools")
devtools::install_github(repo = "stefvanbuuren/mice")
```

See [MICE: Multivariate Imputation by Chained Equations](http://stefvanbuuren.github.io/mice/) for more details.
