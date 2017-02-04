<!-- README.md is generated from README.Rmd. Please edit that file -->
MICE: Multivariate Imputation by Chained Equations
==================================================

The `mice` package implements a method to deal with missing data. The package creates multiple imputations (replacement values) for multivariate missing data. The method is based on Fully Conditional Specification (Van Buuren 2007), where an imputation model for each incomplete variable is created. The MICE algorithm can impute mixes of continuous, binary, unordered categorical and ordered categorical data. In addition, MICE can impute continuous two-level data, and maintain consistency between imputations by means of passive imputation. Many diagnostic plots are implemented to inspect the quality of the imputations.

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

Overview
--------

The `mice` package contains functions to

-   Inspect the missing data pattern
-   Impute the missing data *m* times, resulting in *m* completed data sets
-   Diagnose the quality of the imputed values
-   Analyze each completed data set \_ Pool the results of the repeated analyses
-   Store and export the imputed data in various formats
-   Generate simulated incomplete data

Main functions
--------------

The main functions in the `mice` package are:

| Function name | Description                       |
|---------------|-----------------------------------|
| `mice()`      | Impute the missing data *m* times |
| `with()`      | Analyze completed data sets       |
| `pool()`      | Combine parameter estimates       |
| `complete()`  | Create imputed data sets          |
| `ampute()`    | Generate missing data             |
