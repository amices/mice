# BaBooN

Version: 0.2-0

## In both

*   checking R code for possible problems ... NOTE
    ```
    ...
    BayesBoot: no visible global function definition for ‘runif’
    BayesBoot: no visible global function definition for ‘rmultinom’
    MI.inference: no visible global function definition for ‘var’
    MI.inference: no visible global function definition for ‘qt’
    impChainConversion : <anonymous>: no visible binding for global
      variable ‘var’
    impChainConversion : <anonymous>: no visible binding for global
      variable ‘median’
    impChainConversion : <anonymous>: no visible binding for global
      variable ‘sd’
    rowimpPrep: no visible binding for global variable ‘var’
    summary.impprep: no visible global function definition for ‘head’
    Undefined global functions or variables:
      as.formula binomial glm glm.control head lm median na.exclude na.omit
      predict qt rmultinom runif sd var write.table
    Consider adding
      importFrom("stats", "as.formula", "binomial", "glm", "glm.control",
                 "lm", "median", "na.exclude", "na.omit", "predict", "qt",
                 "rmultinom", "runif", "sd", "var")
      importFrom("utils", "head", "write.table")
    to your NAMESPACE file.
    ```

# brms

Version: 2.7.0

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  6.7Mb
      sub-directories of 1Mb or more:
        R     4.1Mb
        doc   1.8Mb
    ```

# bucky

Version: 1.0.5

## In both

*   checking package dependencies ... NOTE
    ```
    Packages which this enhances but not available for checking:
      ‘texreg’ ‘stargazer’
    ```

*   checking Rd cross-references ... NOTE
    ```
    Package unavailable to check Rd xrefs: ‘mitools’
    ```

# cobalt

Version: 3.6.1

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘backports’
      All declared Imports should be used.
    ```

# dynr

Version: 0.1.13-4

## In both

*   checking whether package ‘dynr’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/buurensv/Package/mice/mice/revdep/checks.noindex/dynr/new/dynr.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
Hi Stef
* installing *source* package ‘dynr’ ...
** package ‘dynr’ successfully unpacked and MD5 sums checked
checking for gsl-config... no
configure: error: gsl-config not found, is GSL installed?
ERROR: configuration failed for package ‘dynr’
* removing ‘/Users/buurensv/Package/mice/mice/revdep/checks.noindex/dynr/new/dynr.Rcheck/dynr’

```
### CRAN

```
Hi Stef
* installing *source* package ‘dynr’ ...
** package ‘dynr’ successfully unpacked and MD5 sums checked
checking for gsl-config... no
configure: error: gsl-config not found, is GSL installed?
ERROR: configuration failed for package ‘dynr’
* removing ‘/Users/buurensv/Package/mice/mice/revdep/checks.noindex/dynr/old/dynr.Rcheck/dynr’

```
# finalfit

Version: 0.9.0

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    Error in re-building vignettes:
      ...
    pandoc: Could not fetch https://www.datasurg.net/wp-content/uploads/2018/07/table1_boot-1024x225.png
    TlsExceptionHostPort (HandshakeFailed (Error_Misc "user error (unexpected type received. expecting handshake and got: Alert [(AlertLevel_Fatal,HandshakeFailure)])")) "www.datasurg.net" 443
    Error: processing vignette 'bootstrap.Rmd' failed with diagnostics:
    pandoc document conversion failed with error 67
    Execution halted
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘readr’
      All declared Imports should be used.
    ```

# Lambda4

Version: 3.0

## In both

*   checking DESCRIPTION meta-information ... NOTE
    ```
    Malformed Title field: should not end in a period.
    ```

*   checking dependencies in R code ... NOTE
    ```
    'library' or 'require' calls in package code:
      ‘GPArotation’ ‘mice’
      Please use :: or requireNamespace() instead.
      See section 'Suggested packages' in the 'Writing R Extensions' manual.
    ```

*   checking R code for possible problems ... NOTE
    ```
    ...
    lambda1: no visible global function definition for ‘cov2cor’
    lambda2: no visible global function definition for ‘cov2cor’
    lambda3: no visible global function definition for ‘cov2cor’
    lambda3: no visible binding for global variable ‘sd’
    lambda5: no visible global function definition for ‘cov2cor’
    lambda6: no visible global function definition for ‘cov2cor’
    omega.tot: no visible global function definition for ‘cov2cor’
    omega.tot: no visible global function definition for ‘factanal’
    quant.lambda4: no visible global function definition for ‘cov2cor’
    quant.lambda4: no visible global function definition for ‘runif’
    quant.lambda4: no visible global function definition for ‘quantile’
    raju: no visible global function definition for ‘cov2cor’
    raju: no visible global function definition for ‘runif’
    user.lambda4: no visible global function definition for ‘runif’
    user.lambda4: no visible binding for global variable ‘sd’
    Undefined global functions or variables:
      complete cov cov2cor factanal median mice quantile runif sd
    Consider adding
      importFrom("stats", "cov", "cov2cor", "factanal", "median", "quantile",
                 "runif", "sd")
    to your NAMESPACE file.
    ```

# LSAmitR

Version: 1.0-2

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 55 marked UTF-8 strings
    ```

# miceFast

Version: 0.2.3

## In both

*   checking whether package ‘miceFast’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/buurensv/Package/mice/mice/revdep/checks.noindex/miceFast/new/miceFast.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
Hi Stef
* installing *source* package ‘miceFast’ ...
** package ‘miceFast’ successfully unpacked and MD5 sums checked
** libs
clang++ -std=gnu++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/buurensv/Package/mice/mice/revdep/library.noindex/mice/new/Rcpp/include" -I"/Users/buurensv/Package/mice/mice/revdep/library.noindex/miceFast/RcppArmadillo/include" -I/usr/local/include  -fopenmp -fPIC  -Wall -g -O2 -c R_funs.cpp -o R_funs.o
clang: error: unsupported option '-fopenmp'
make: *** [R_funs.o] Error 1
ERROR: compilation failed for package ‘miceFast’
* removing ‘/Users/buurensv/Package/mice/mice/revdep/checks.noindex/miceFast/new/miceFast.Rcheck/miceFast’

```
### CRAN

```
Hi Stef
* installing *source* package ‘miceFast’ ...
** package ‘miceFast’ successfully unpacked and MD5 sums checked
** libs
clang++ -std=gnu++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/buurensv/Package/mice/mice/revdep/library.noindex/mice/old/Rcpp/include" -I"/Users/buurensv/Package/mice/mice/revdep/library.noindex/miceFast/RcppArmadillo/include" -I/usr/local/include  -fopenmp -fPIC  -Wall -g -O2 -c R_funs.cpp -o R_funs.o
clang: error: unsupported option '-fopenmp'
make: *** [R_funs.o] Error 1
ERROR: compilation failed for package ‘miceFast’
* removing ‘/Users/buurensv/Package/mice/mice/revdep/checks.noindex/miceFast/old/miceFast.Rcheck/miceFast’

```
# MissingDataGUI

Version: 0.2-5

## In both

*   checking package dependencies ... ERROR
    ```
    Packages required but not available: ‘gWidgetsRGtk2’ ‘cairoDevice’
    
    See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
    manual.
    ```

# miWQS

Version: 0.0.9

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘graphics’
      All declared Imports should be used.
    ```

# MRPC

Version: 2.0.0

## In both

*   checking whether package ‘MRPC’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/buurensv/Package/mice/mice/revdep/checks.noindex/MRPC/new/MRPC.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
Hi Stef
* installing *source* package ‘MRPC’ ...
** package ‘MRPC’ successfully unpacked and MD5 sums checked
** R
** data
*** moving datasets to lazyload DB
** byte-compile and prepare package for lazy loading
Error in loadNamespace(i, c(lib.loc, .libPaths()), versionCheck = vI[[i]]) : 
  there is no package called ‘GO.db’
ERROR: lazy loading failed for package ‘MRPC’
* removing ‘/Users/buurensv/Package/mice/mice/revdep/checks.noindex/MRPC/new/MRPC.Rcheck/MRPC’

```
### CRAN

```
Hi Stef
* installing *source* package ‘MRPC’ ...
** package ‘MRPC’ successfully unpacked and MD5 sums checked
** R
** data
*** moving datasets to lazyload DB
** byte-compile and prepare package for lazy loading
Error in loadNamespace(i, c(lib.loc, .libPaths()), versionCheck = vI[[i]]) : 
  there is no package called ‘GO.db’
ERROR: lazy loading failed for package ‘MRPC’
* removing ‘/Users/buurensv/Package/mice/mice/revdep/checks.noindex/MRPC/old/MRPC.Rcheck/MRPC’

```
# NNLM

Version: 0.4.2

## In both

*   checking whether package ‘NNLM’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/buurensv/Package/mice/mice/revdep/checks.noindex/NNLM/new/NNLM.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
Hi Stef
* installing *source* package ‘NNLM’ ...
** package ‘NNLM’ successfully unpacked and MD5 sums checked
** libs
clang++  -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -fopenmp -I"/Users/buurensv/Package/mice/mice/revdep/library.noindex/mice/new/Rcpp/include" -I"/Users/buurensv/Package/mice/mice/revdep/library.noindex/NNLM/RcppArmadillo/include" -I"/Users/buurensv/Package/mice/mice/revdep/library.noindex/NNLM/RcppProgress/include" -I/usr/local/include   -fPIC  -Wall -g -O2  -c RcppExports.cpp -o RcppExports.o
clang: error: unsupported option '-fopenmp'
make: *** [RcppExports.o] Error 1
ERROR: compilation failed for package ‘NNLM’
* removing ‘/Users/buurensv/Package/mice/mice/revdep/checks.noindex/NNLM/new/NNLM.Rcheck/NNLM’

```
### CRAN

```
Hi Stef
* installing *source* package ‘NNLM’ ...
** package ‘NNLM’ successfully unpacked and MD5 sums checked
** libs
clang++  -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -fopenmp -I"/Users/buurensv/Package/mice/mice/revdep/library.noindex/mice/old/Rcpp/include" -I"/Users/buurensv/Package/mice/mice/revdep/library.noindex/NNLM/RcppArmadillo/include" -I"/Users/buurensv/Package/mice/mice/revdep/library.noindex/NNLM/RcppProgress/include" -I/usr/local/include   -fPIC  -Wall -g -O2  -c RcppExports.cpp -o RcppExports.o
clang: error: unsupported option '-fopenmp'
make: *** [RcppExports.o] Error 1
ERROR: compilation failed for package ‘NNLM’
* removing ‘/Users/buurensv/Package/mice/mice/revdep/checks.noindex/NNLM/old/NNLM.Rcheck/NNLM’

```
# rattle

Version: 5.2.0

## In both

*   checking package dependencies ... NOTE
    ```
    Packages suggested but not available for checking:
      ‘cairoDevice’ ‘gWidgetsRGtk2’ ‘playwith’ ‘rggobi’ ‘RGtk2’
      ‘RGtk2Extras’
    ```

*   checking installed package size ... NOTE
    ```
      installed size is  7.8Mb
      sub-directories of 1Mb or more:
        data   3.0Mb
        etc    1.9Mb
        po     1.2Mb
    ```

# rexposome

Version: 1.2.0

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    ...
      Failed to tidy R code in chunk 'plot_enet_heatmap'. Reason:
    Error in loadNamespace(name) : there is no package called 'formatR'
    
    Loading required package: lattice
    
    Attaching package: 'mice'
    
    The following objects are masked from 'package:BiocGenerics':
    
        cbind, rbind
    
    The following objects are masked from 'package:base':
    
        cbind, rbind
    
    Warning in has_utility("convert", "ImageMagick") :
      ImageMagick not installed or not in PATH
    Quitting from lines 229-231 (mutiple_imputation_data_analysis.Rmd) 
    Error: processing vignette 'mutiple_imputation_data_analysis.Rmd' failed with diagnostics:
    'names' attribute [4] must be the same length as the vector [0]
    Execution halted
    ```

*   checking installed package size ... NOTE
    ```
      installed size is 11.0Mb
      sub-directories of 1Mb or more:
        data      2.8Mb
        doc       5.1Mb
        extdata   2.2Mb
    ```

*   checking R code for possible problems ... NOTE
    ```
    imputeLOD: multiple local function definitions for ‘faux’ with
      different formal arguments
    plotHistogram,ExposomeSet: no visible binding for global variable
      ‘..density..’
    Undefined global functions or variables:
      ..density..
    ```

# roughrf

Version: 1.0

## In both

*   checking R code for possible problems ... NOTE
    ```
    mfix : m4m: no visible global function definition for ‘median’
    rrfa: no visible global function definition for ‘predict’
    rrfb: no visible global function definition for ‘predict’
    rrfc1: no visible global function definition for ‘predict’
    rrfc2: no visible global function definition for ‘predict’
    rrfc3: no visible global function definition for ‘predict’
    rrfc4: no visible global function definition for ‘predict’
    rrfc5 : regdat: no visible global function definition for ‘predict’
    rrfc5 : regdat: no visible global function definition for ‘glm’
    rrfc5: no visible global function definition for ‘predict’
    rrfc6: no visible global function definition for ‘predict’
    rrfc7: no visible global function definition for ‘predict’
    rrfd: no visible global function definition for ‘predict’
    rrfe : rdms: no visible global function definition for ‘rbinom’
    rrfe: no visible global function definition for ‘predict’
    Undefined global functions or variables:
      glm median predict rbinom
    Consider adding
      importFrom("stats", "glm", "median", "predict", "rbinom")
    to your NAMESPACE file.
    ```

# smartdata

Version: 1.0.2

## In both

*   checking examples ... ERROR
    ```
    ...
    +     tree <- rpart(FSelector::as.simple.formula(subset, "Species"), train)
    +     error.rate = sum(test$Species != predict(tree, test, type="c")) / nrow(test)
    +     return(1 - error.rate)
    +   })
    +   print(subset)
    +   print(mean(results))
    +   return(mean(results))
    + }
    > 
    > 
    > 
    > super_iris <- feature_selection(iris, "Boruta", class_attr = "Species")
    > super_iris <- feature_selection(iris, "chi_squared",
    +                                 class_attr = "Species", num_features = 3)
    Error: .onLoad failed in loadNamespace() for 'rJava', details:
      call: dyn.load(file, DLLpath = DLLpath, ...)
      error: unable to load shared object '/Users/buurensv/Package/mice/mice/revdep/library.noindex/smartdata/rJava/libs/rJava.so':
      dlopen(/Users/buurensv/Package/mice/mice/revdep/library.noindex/smartdata/rJava/libs/rJava.so, 6): Library not loaded: /Library/Java/JavaVirtualMachines/jdk-11.0.1.jdk/Contents/Home/lib/server/libjvm.dylib
      Referenced from: /Users/buurensv/Package/mice/mice/revdep/library.noindex/smartdata/rJava/libs/rJava.so
      Reason: image not found
    Execution halted
    ```

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      ══ testthat results  ════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════
      OK: 306 SKIPPED: 0 FAILED: 23
      1. Failure: Correct feature selection (@testFeatSelection.R#20) 
      2. Failure: Correct feature selection (@testFeatSelection.R#21) 
      3. Failure: Correct feature selection (@testFeatSelection.R#24) 
      4. Failure: Correct feature selection (@testFeatSelection.R#25) 
      5. Failure: Correct feature selection (@testFeatSelection.R#26) 
      6. Failure: Correct feature selection (@testFeatSelection.R#27) 
      7. Failure: Correct feature selection (@testFeatSelection.R#28) 
      8. Failure: Correct noise treatment (@testNoise.R#13) 
      9. Failure: Correct noise treatment (@testNoise.R#14) 
      1. ...
      
      Error: testthat unit tests failed
      Execution halted
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘Amelia’ ‘Boruta’ ‘DMwR’ ‘MVN’ ‘NoiseFiltersR’ ‘VIM’ ‘adaptiveGPCA’
      ‘class’ ‘clusterSim’ ‘denoiseR’ ‘discretization’ ‘imbalance’ ‘lle’
      ‘missForest’ ‘missMDA’ ‘outliers’ ‘unbalanced’
      All declared Imports should be used.
    ```

