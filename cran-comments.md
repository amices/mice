cran-comments
================

Package built by

``` r
library("devtools")
build_vignettes()
build()
```

`"/Users/buurensv/Package/mice/mice_2.46.0.tar.gz"`

Test environments
-----------------

-   local OS X install, 3.4.2
-   win-builder (devel and release)

R CMD check results
-------------------

There were no ERRORs or WARNINGs.

There was 1 NOTE:

-   Version jumps in minor (submitted: 2.46.0, existing: 2.30)

Downstream dependencies
-----------------------

I have run

``` r
revdep_check()
revdep_check_save_summary()
revdep_check_print_problems()
```

There were 40 reverse dependencies.

7 packages with problems

| package        | version  |  errors|  warnings|  notes|
|:---------------|:---------|-------:|---------:|------:|
| dynr           | 0.1.11-8 |       1|         0|      0|
| Hmisc          | 4.0-3    |       1|         0|      0|
| MissingDataGUI | 0.2-5    |       1|         0|      0|
| NNLM           | 0.4.1    |       1|         0|      0|
| Qtools         | 1.2      |       1|         0|      0|
| rattle         | 5.1.0    |       1|         0|      0|
| weightTAPSPACK | 0.1      |       1|         0|      0|

-   Installation failed: dynr, Hmisc, NNLM, Qtools
-   Packages required but not available: MissingDataGUI, rattle, weightTAPSPACK
-   No other ERRORs found.
