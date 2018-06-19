cran-comments
================

Package built by

``` r
library("devtools")
build()
```

`"/Users/buurensv/Package/mice/mice_3.0.2.tar.gz"`

Test environments
-----------------

-   local OS X install, 10.13.4, R 3.5.0
-   win-builder, using `devtools::build_win()`

Status: OK

R CMD check results
-------------------

local checks of tarball fails:

`$ R CMD CHECK mice_3.1.0.tar.gz`

yields: `Error: package ‘Rcpp’ was installed by an R version with different internals; it needs to be reinstalled for use with this R version Execution halted`

(I have installed `Rccp 0.12.17`)

`Error: package or namespace load failed for ‘lattice’:  package ‘lattice’ was installed by an R version with different internals; it needs to be reinstalled for use with this R version`

(I have installed `lattice 0.20-35`)

Reinstalling does not help (`warning: package ‘Rccp’ is not available (for R version 3.5.0)`). I cannot repair this problem. I have assumed this is a temporary flitch that we may safely ignore for now.

Downstream dependencies
-----------------------

I have run

``` r
revdep_check()
revdep_check_save_summary()
revdep_check_print_problems()
```

There were 47 reverse dependencies.

10 packages with problems + actions

| package        | version  |  errors|  warnings|  notes| My actions                                           |
|:---------------|:---------|-------:|---------:|------:|:-----------------------------------------------------|
| dynr           | 0.1.12-5 |       1|         0|      0| needs additional local installs, not further checked |
| Hmisc          | 4.1-1    |       1|         0|      0| installed by hand                                    |
| JointAI        | 0.1.0    |       1|         0|      0| depends on external JAGS, not further checked        |
| logistf        | 1.22     |       1|         0|      1| summary(pool.RR(fit.list)) error                     |
| miceFast       | 0.2.3    |       1|         0|      0| C compilation errors                                 |
| MissingDataGUI | 0.2-5    |       1|         0|      0| 'gWidgetsRGtk2', 'cairoDevice' not available         |
| NNLM           | 0.4.2    |       1|         0|      0| fails to install                                     |
| Qtools         | 1.3      |       1|         0|      0| fortran compilation errors, author noted             |
| rattle         | 5.1.0    |       1|         0|      0| 'cairoDevice' not available                          |
| weightTAPSPACK | 0.1      |       1|         0|      0| package "HotDeckImputation" not available            |

-   Failed to install dependencies for: MissingDataGUI, rattle, weightTAPSPACK
-   Failed to install: dynr, Hmisc, JointAI, miceFast, NNLM, Qtools
-   logistf: checking examples ... ERROR

See <https://github.com/stefvanbuuren/mice/blob/master/revdep/problems.md>
