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
-   Analyze each completed data set
-   Pool the results of the repeated analyses
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

One-day course in `mice`
------------------------

If you are new to `mice` consider studying the materials of our one-day course in Winnigpeg. The materials are at [https://stefvanbuuren.github.io/Winnipeg](https://stefvanbuuren.github.io/Winnipeg/).

miceVignettes
-------------

A detailed series of vignettes that walk you through solving realistic inference problems with mice. These vignettes overlap with the one-day course.

We suggest going through these vignettes in the following order

1.  [Ad hoc methods and the MICE algorithm](https://gerkovink.github.io/miceVignettes/Ad_hoc_and_mice/Ad_hoc_methods.html)
2.  [Convergence and pooling](https://gerkovink.github.io/miceVignettes/Convergence_pooling/Convergence_and_pooling.html)
3.  [Inspecting how the observed data and missingness are related](https://gerkovink.github.io/miceVignettes/Missingness_inspection/Missingness_inspection.html)
4.  [Passive imputation and post-processing](https://gerkovink.github.io/miceVignettes/Passive_Post_processing/Passive_imputation_post_processing.html)
5.  [Imputing multilevel data](https://gerkovink.github.io/miceVignettes/Multi_level/Multi_level_data.html)
6.  [Sensitivity analysis with `mice`](https://gerkovink.github.io/miceVignettes/Sensitivity_analysis/Sensitivity_analysis.html)

Related packages
----------------

Some packages that extend the functionality of `mice` include:

1.  [`ImputeRobust`: Multiple Imputation with `GAMLSS`](https://github.com/dsalfran/ImputeRobust)
2.  [`countimp`: Incomplete count data](https://github.com/kkleinke/countimp)
3.  [`miceadds`: Functions for multilevel imputation](https://cran.r-project.org/packages=miceadds)
4.  [`micemd`: Functions for multilevel imputation](https://cran.r-project.org/package=micemd)
5.  [`CALIBERrfimpute`: Another random forest method](https://cran.r-project.org/package=CALIBERrfimpute)
6.  [`parlMICE`: Parallel MICE imputation wrapper](https://gerkovink.github.io/parlMICE/Vignette_parlMICE.html)
7.  [`fancyimpyute`: MICE in Python for ordinal data](https://github.com/hammerlab/fancyimpute)

Further reading
---------------

The `mice` [software](https://www.jstatsoft.org/v45/i03/paper) was published in the Journal of Statistical Software (Buuren and Groothuis-Oudshoorn 2011). The first application of the method concerned [missing blood pressure](http://www.stefvanbuuren.nl/publications/Multiple%20imputation%20-%20Stat%20Med%201999.pdf) data (Buuren, Boshuizen, and Knook 1999). The term [Fully Conditional Specification](http://www.stefvanbuuren.nl/publications/FCS%20in%20multivariate%20imputation%20-%20JSCS%202006.pdf) was introduced in 2006 to describe a general class of methods that specify imputations model for multivariate data as a set of conditional distributions (Buuren et al. 2006). Details about imputing [mixes of numerical and categorical data](http://www.stefvanbuuren.nl/publications/MI%20by%20FCS%20-%20SMMR%202007.pdf) can be found in (Buuren 2007). Many more details and applications can be found in the book *Flexible Imputation of Missing Data* (Buuren 2012) (see [Chapman & Hall/CRC](https://www.crcpress.com/Flexible-Imputation-of-Missing-Data/van-Buuren/p/book/9781439868249) or [Amazon](https://www.amazon.com/Flexible-Imputation-Missing-Interdisciplinary-Statistics/dp/1439868247/ref=pd_sim_14_2?_encoding=UTF8&pd_rd_i=1439868247&pd_rd_r=DC8AYZZZZKF9GZSVP6PK&pd_rd_w=Z2PJQ&pd_rd_wg=3iuSi&psc=1&refRID=DC8AYZZZZKF9GZSVP6PK)).

References
----------

Buuren, S. van. 2007. “Multiple Imputation of Discrete and Continuous Data by Fully Conditional Specification.” *Statistical Methods in Medical Research* 16 (3): 219–42.

———. 2012. *Flexible Imputation of Missing Data*. Boca Raton, FL: Chapman & Hall/CRC Press.

Buuren, S. van, and K. Groothuis-Oudshoorn. 2011. “MICE: Multivariate Imputation by Chained Equations in R.” *Journal of Statistical Software* 45 (3): 1–67.

Buuren, S. van, H. C. Boshuizen, and D. L. Knook. 1999. “Multiple Imputation of Missing Blood Pressure Covariates in Survival Analysis.” *Statistics in Medicine* 18 (6): 681–94.

Buuren, S. van, J. P. L. Brand, C. G. M. Groothuis-Oudshoorn, and D. B. Rubin. 2006. “Fully Conditional Specification in Multivariate Imputation.” *Journal of Statistical Computation and Simulation* 76 (12): 1049–64.
