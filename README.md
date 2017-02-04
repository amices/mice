<!-- README.md is generated from README.Rmd. Please edit that file -->
MICE: Multivariate Imputation by Chained Equations
==================================================

The `mice` package implements a method to deal with missing data. The package creates multiple imputations (replacement values) for multivariate missing data. The method is based on Fully Conditional Specification, where each incomplete variable is imputed by a separate model. The `MICE` algorithm can impute mixes of continuous, binary, unordered categorical and ordered categorical data. In addition, MICE can impute continuous two-level data, and maintain consistency between imputations by means of passive imputation. Many diagnostic plots are implemented to inspect the quality of the imputations.

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
-   Incorporate custom imputation methods

Main functions
--------------

The main functions in the `mice` package are:

| Function name | Description                       |
|---------------|-----------------------------------|
| `mice()`      | Impute the missing data *m* times |
| `with()`      | Analyze completed data sets       |
| `pool()`      | Combine parameter estimates       |
| `complete()`  | Export imputed data               |
| `ampute()`    | Generate missing data             |

Further reading
---------------

The `mice` software was published in the Journal of Statistical Software (Buuren and Groothuis-Oudshoorn 2011). See <https://www.jstatsoft.org/article/view/v045i03>. The first application of the method concerned missing blood pressure data (Buuren, Boshuizen, and Knook 1999). The term *Fully Conditional Specification* was introduced in 2006 to describe a general class of methods that specify imputations model for multivariate data as a set of conditional distributions (Buuren et al. 2006). Further details and applications can be found in the book *Flexible Imputation of Missing Data* (Buuren 2012).

References
----------

Buuren, S. van. 2012. *Flexible Imputation of Missing Data*. Boca Raton, FL: Chapman & Hall/CRC Press.

Buuren, S. van, and K. Groothuis-Oudshoorn. 2011. “MICE: Multivariate Imputation by Chained Equations in R.” *Journal of Statistical Software* 45 (3): 1–67.

Buuren, S. van, H. C. Boshuizen, and D. L. Knook. 1999. “Multiple Imputation of Missing Blood Pressure Covariates in Survival Analysis.” *Statistics in Medicine* 18 (6): 681–94.

Buuren, S. van, J. P. L. Brand, C. G. M. Groothuis-Oudshoorn, and D. B. Rubin. 2006. “Fully Conditional Specification in Multivariate Imputation.” *Journal of Statistical Computation and Simulation* 76 (12): 1049–64.
