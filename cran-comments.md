cran-comments
================

Package built by

``` r
library("devtools")
build()
```

`"/Users/buurensv/Package/mice/mice_3.2.0.tar.gz"`

Test environments
-----------------

-   local OS X install, 10.13.6, R 3.5.1
-   win-builder, using `devtools::build_win()`

Status: OK

R CMD check results
-------------------

local checks of tarball:

`$ R CMD CHECK mice_3.2.0.tar.gz`

Status: OK

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
| cobalt         | 3.3.0    |       0|         1|      1| mising `designmatch` package                         |
| dynr           | 0.1.12-5 |       1|         0|      0| needs additional local installs, not further checked |
| Hmisc          | 4.1-1    |       1|         0|      0| installed by hand                                    |
| JointAI        | 0.1.0    |       1|         0|      0| depends on external JAGS, not further checked        |
| miceFast       | 0.2.3    |       1|         0|      0| C compilation errors                                 |
| MissingDataGUI | 0.2-5    |       1|         0|      0| 'gWidgetsRGtk2', 'cairoDevice' not available         |
| NNLM           | 0.4.2    |       1|         0|      0| fails to install                                     |
| Qtools         | 1.3      |       1|         0|      0| fortran compilation errors, author noted             |
| rattle         | 5.1.0    |       1|         0|      0| 'cairoDevice' not available                          |
| weightTAPSPACK | 0.1      |       1|         0|      0| package "HotDeckImputation" not available            |

-   Failed to install dependencies for: cobalt, MissingDataGUI, rattle, weightTAPSPACK
-   Failed to install: dynr, Hmisc, JointAI, miceFast, NNLM, Qtools

See <https://github.com/stefvanbuuren/mice/blob/master/revdep/problems.md>
