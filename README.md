<!-- README.md is generated from README.Rmd. Please edit that file -->
[MICE: Multivariate Imputation by Chained Equations](http://stefvanbuuren.github.io/mice/)
==========================================================================================

The [`mice`](https://cran.r-project.org/package=mice) package implements a method to deal with missing data. The package creates multiple imputations (replacement values) for multivariate missing data.

The `dev` branch contains code that reflect some new features that should eventually end up in `mice 3`. The following features and currently implemented:

1.  `blocks`: The main algorithm iterates over blocks. A block is simply a collection of variables. In the old MICE algorithm each block was equivalent to one variable, which - of course - is still the default; The `blocks` argument allows mixing univariate imputation method multivariate imputation methods. The `blocks` feature bridges two seemingly disparate approaches, joint modeling and fully conditional specification, into one framework;
2.  `where`: The `where` argument is a logical matrix of the same size of `data` that specifies which cells should be imputed. This opens up some new analytic possibilities (which are still to be documented);
3.  Multivariate tests: There are new functions `D1()`, `D2()`, `D3()` and `anova()` that perform multivariate parameter tests on the repeated analysis from on multiply-imputed data;
4.  `formulas`: The old `form` argument has been redesign and is now renamed to `formulas`. This provides an alternative way to specify imputation models that exploits the full power of R's native formula's.

Things on the wish list are:

1.  Better support for, and integration with, the `tidyverse` framework, especially for the packages `dplyr`, `tibble` and `broom`;
2.  Methods for automatic specification of imputation models;
3.  Easier specification of models for data that are Missing Not at Random (MNAR) and sensitivity analysis;
4.  Functionality for testing the quality of `mice.impute.xxx()` functions;
5.  Better numerical algorithms for low-level imputation function. Better handling of duplicate variables.
6.  Better documentation. Of everything...

I'll be happy to take feedback and discuss suggestions. Please submit these through Github's issues facility.

Installation
------------

The `mice` package can be installed from CRAN as follows:

``` r
install.packages("mice")
```

The latest version is can be installed from GitHub as follows:

``` r
install.packages("devtools")
devtools::install_github(repo = "stefvanbuuren/mice", ref = "dev")
```

See [MICE: Multivariate Imputation by Chained Equations](http://stefvanbuuren.github.io/mice/) for more details.
