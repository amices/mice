cran-comments
================

Package built by

``` r
library("devtools")
build_vignettes()
build()
```

`"/Users/buurensv/Package/mice/mice_2.47.1.9112.tar.gz"`

Test environments
-----------------

-   local OS X install, 10.13.4, R 3.5
-   win-builder (devel and release)

R CMD check results
-------------------

There were no ERRORs or WARNINGs or NOTEs.

Downstream dependencies
-----------------------

I have run

``` r
revdep_check()
revdep_check_save_summary()
revdep_check_print_problems()
```

There were 48 reverse dependencies.

Check results
=============

19 packages with problems

<table>
<colgroup>
<col width="19%" />
<col width="11%" />
<col width="9%" />
<col width="11%" />
<col width="8%" />
<col width="40%" />
</colgroup>
<thead>
<tr class="header">
<th align="left">package</th>
<th align="left">version</th>
<th align="right">errors</th>
<th align="right">warnings</th>
<th align="right">notes</th>
<th align="left">My actions</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td align="left">brms</td>
<td align="left">2.3.0</td>
<td align="right">1</td>
<td align="right">0</td>
<td align="right">2</td>
<td align="left">no error in brms 2.3.1</td>
</tr>
<tr class="even">
<td align="left">CALIBERrfimpute</td>
<td align="left">0.1-6</td>
<td align="right">0</td>
<td align="right">1</td>
<td align="right">1</td>
<td align="left">showtable() in simstudy_survival defunct - author noted</td>
</tr>
<tr class="odd">
<td align="left">codebook</td>
<td align="left">0.5.8</td>
<td align="right">1</td>
<td align="right">0</td>
<td align="right">1</td>
<td align="left"><a href="https://github.com/rubenarslan/codebook/issues/20" class="uri">https://github.com/rubenarslan/codebook/issues/20</a></td>
</tr>
<tr class="even">
<td align="left">dlookr</td>
<td align="left">0.3.0</td>
<td align="right">1</td>
<td align="right">0</td>
<td align="right">0</td>
<td align="left">author noted</td>
</tr>
<tr class="odd">
<td align="left">dynr</td>
<td align="left">0.1.12-5</td>
<td align="right">1</td>
<td align="right">0</td>
<td align="right">0</td>
<td align="left">author noted, needs additional local installs, not further checked</td>
</tr>
<tr class="even">
<td align="left">HardyWeinberg</td>
<td align="left">1.5.9</td>
<td align="right">0</td>
<td align="right">1</td>
<td align="right">0</td>
<td align="left">asked author whether error could be correct</td>
</tr>
<tr class="odd">
<td align="left">Hmisc</td>
<td align="left">4.1-1</td>
<td align="right">1</td>
<td align="right">0</td>
<td align="right">0</td>
<td align="left">installed by hand</td>
</tr>
<tr class="even">
<td align="left">hot.deck</td>
<td align="left">1.1</td>
<td align="right">0</td>
<td align="right">1</td>
<td align="right">0</td>
<td align="left">warning unrelated to mice</td>
</tr>
<tr class="odd">
<td align="left">HSAUR3</td>
<td align="left">1.0-8</td>
<td align="right">0</td>
<td align="right">1</td>
<td align="right">0</td>
<td align="left">solved by reverting print.mira</td>
</tr>
<tr class="even">
<td align="left">JointAI</td>
<td align="left">0.1.0</td>
<td align="right">1</td>
<td align="right">0</td>
<td align="right">0</td>
<td align="left">depends on external JAGS, not further checked</td>
</tr>
<tr class="odd">
<td align="left">logistf</td>
<td align="left">1.22</td>
<td align="right">1</td>
<td align="right">0</td>
<td align="right">1</td>
<td align="left">pooling code not compatible - author noted</td>
</tr>
<tr class="even">
<td align="left">miceadds</td>
<td align="left">2.11-80</td>
<td align="right">1</td>
<td align="right">0</td>
<td align="right">0</td>
<td align="left"><a href="https://github.com/alexanderrobitzsch/miceadds/issues/6" class="uri">https://github.com/alexanderrobitzsch/miceadds/issues/6</a></td>
</tr>
<tr class="odd">
<td align="left">miceFast</td>
<td align="left">0.2.3</td>
<td align="right">1</td>
<td align="right">0</td>
<td align="right">0</td>
<td align="left">C compilation errors</td>
</tr>
<tr class="even">
<td align="left">miceMNAR</td>
<td align="left">1.0</td>
<td align="right">1</td>
<td align="right">0</td>
<td align="right">0</td>
<td align="left">incompatible mice.impute.hecknorm(), author noted</td>
</tr>
<tr class="odd">
<td align="left">MissingDataGUI</td>
<td align="left">0.2-5</td>
<td align="right">1</td>
<td align="right">0</td>
<td align="right">0</td>
<td align="left">'gWidgetsRGtk2', 'cairoDevice' not available</td>
</tr>
<tr class="even">
<td align="left">Qtools</td>
<td align="left">1.3</td>
<td align="right">1</td>
<td align="right">0</td>
<td align="right">0</td>
<td align="left">fortran compilation errors</td>
</tr>
<tr class="odd">
<td align="left">rattle</td>
<td align="left">5.1.0</td>
<td align="right">1</td>
<td align="right">0</td>
<td align="right">0</td>
<td align="left">'cairoDevice' not available</td>
</tr>
<tr class="even">
<td align="left">sjmisc</td>
<td align="left">2.7.2</td>
<td align="right">1</td>
<td align="right">0</td>
<td align="right">0</td>
<td align="left">no error sjmisc 2.7.2</td>
</tr>
<tr class="odd">
<td align="left">weightTAPSPACK</td>
<td align="left">0.1</td>
<td align="right">1</td>
<td align="right">0</td>
<td align="right">0</td>
<td align="left">compilation error in Rglpk</td>
</tr>
</tbody>
</table>

brms (2.3.0)
------------

Maintainer: Paul-Christian Bürkner <paul.buerkner@gmail.com>
Bug reports: <https://github.com/paul-buerkner/brms/issues>

1 error | 0 warnings | 2 notes

    checking tests ... ERROR
      Running ‘testthat.R’ [81s/82s]
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      ══ testthat results  ══════════════════════════════════════════════════════════════
      OK: 995 SKIPPED: 1 FAILED: 19
      1. Failure: brm produces expected errors (@tests.brm.R#17) 
      2. Error: all S3 methods have reasonable ouputs (@tests.brmsfit-methods.R#235) 
      3. Error: self-defined functions appear in the Stan code (@tests.make_stancode.R#334) 
      4. Failure: invalid combinations of modeling options are detected (@tests.make_stancode.R#403) 
      5. Failure: invalid combinations of modeling options are detected (@tests.make_stancode.R#411) 
      6. Error: Stan code for multivariate models is correct (@tests.make_stancode.R#454) 
      7. Error: known standard errors appear in the Stan code (@tests.make_stancode.R#701) 
      8. Error: Stan code of response times models is correct (@tests.make_stancode.R#802) 
      9. Error: weighted, censored, and truncated likelihoods are correct (@tests.make_stancode.R#896) 
      1. ...
      
      Error: testthat unit tests failed
      Execution halted

    checking installed package size ... NOTE
      installed size is  8.2Mb
      sub-directories of 1Mb or more:
        R     5.1Mb
        doc   2.4Mb

    checking R code for possible problems ... NOTE
    combine_family_info: no visible binding for global variable ‘isFALSE’
    no_sigma: no visible global function definition for ‘isFALSE’
    resp_cens : prepare_cens: no visible global function definition for
      ‘isFALSE’
    Undefined global functions or variables:
      isFALSE

CALIBERrfimpute (0.1-6)
-----------------------

Maintainer: Anoop Shah <anoop@doctors.org.uk>

0 errors | 1 warning | 1 note

    checking re-building of vignette outputs ... WARNING
    Error in re-building vignettes:
      ...
    Loading required package: mice
    Loading required package: lattice

    Attaching package: ‘mice’

    The following objects are masked from ‘package:base’:

        cbind, rbind

    Loading required package: randomForest
    randomForest 4.6-14
    Type rfNews() to see new features/changes/bug fixes.
    Loading required package: foreach
    Loading required package: itertools
    Loading required package: iterators
    Warning in parallel::mclapply(1:N, doanalysis) :
      all scheduled cores encountered errors in user code

    Error: processing vignette 'simstudy_survival.Rnw' failed with diagnostics:
     chunk 9 
    Error in x[[method]] : subscript out of bounds
    Execution halted


    checking R code for possible problems ... NOTE
    makemar : predictions: no visible global function definition for
      ‘rbinom’
    mice.impute.rfcat : <anonymous>: no visible global function definition
      for ‘predict’
    mice.impute.rfcont: no visible global function definition for ‘predict’
    mice.impute.rfcont: no visible global function definition for ‘rnorm’
    simdata: no visible global function definition for ‘rbinom’
    simdata: no visible global function definition for ‘rnorm’
    Undefined global functions or variables:
      predict rbinom rnorm
    Consider adding
      importFrom("stats", "predict", "rbinom", "rnorm")
    to your NAMESPACE file.

codebook (0.5.8)
----------------

Maintainer: Ruben Arslan <ruben.arslan@gmail.com>
Bug reports: <https://github.com/rubenarslan/codebook/issues>

1 error | 0 warnings | 1 note

    checking examples ... ERROR
    Running examples in ‘codebook-Ex.R’ failed
    The error most likely occurred in:

    > base::assign(".ptime", proc.time(), pos = "CheckExEnv")
    > ### Name: codebook_missingness
    > ### Title: Codebook missingness
    > ### Aliases: codebook_missingness
    > 
    > ### ** Examples
    > 
    > data("bfi")
    > codebook_missingness(bfi)
    Error in `rownames<-`(`*tmp*`, value = table(pat)) : 
      attempt to set 'rownames' on an object with no dimensions
    Calls: codebook_missingness -> md_pattern -> <Anonymous> -> rownames<-
    Execution halted

    checking Rd cross-references ... NOTE
    Package unavailable to check Rd xrefs: ‘labelled’

dlookr (0.3.0)
--------------

Maintainer: Choonghyun Ryu <choonghyun.ryu@gmail.com>

1 error | 0 warnings | 0 notes

    checking package dependencies ... ERROR
    Packages required but not available:
      ‘RcmdrMisc’ ‘corrplot’ ‘classInt’ ‘moments’ ‘kableExtra’ ‘prettydoc’
      ‘smbinning’ ‘DMwR’

    Package suggested but not available for checking: ‘ISLR’

    See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
    manual.

dynr (0.1.12-5)
---------------

Maintainer: Michael D. Hunter <mhunter.ou@gmail.com>

1 error | 0 warnings | 0 notes

    checking whether package ‘dynr’ can be installed ... ERROR
    Installation failed.
    See ‘/Users/buurensv/Package/mice/mice/revdep/checks/dynr.Rcheck/00install.out’ for details.

HardyWeinberg (1.5.9)
---------------------

Maintainer: Jan Graffelman <jan.graffelman@upc.edu>

0 errors | 1 warning | 0 notes

    checking re-building of vignette outputs ... WARNING
    Error in re-building vignettes:
      ...
    Loading required package: mice
    Loading required package: lattice

    Attaching package: 'mice'

    The following objects are masked from 'package:base':

        cbind, rbind

    Loading required package: Rsolnp

    Error: processing vignette 'HardyWeinberg.Rnw' failed with diagnostics:
     chunk 13 
    Error in edit.setup(data, setup, ...) : nothing left to impute
    Execution halted

Hmisc (4.1-1)
-------------

Maintainer: Frank E Harrell Jr <f.harrell@vanderbilt.edu>

1 error | 0 warnings | 0 notes

    checking whether package ‘Hmisc’ can be installed ... ERROR
    Installation failed.
    See ‘/Users/buurensv/Package/mice/mice/revdep/checks/Hmisc.Rcheck/00install.out’ for details.

hot.deck (1.1)
--------------

Maintainer: Dave Armstrong <dave@quantoid.net>

0 errors | 1 warning | 0 notes

    checking re-building of vignette outputs ... WARNING
    Error in re-building vignettes:
      ...
    Loading required package: mice
    Loading required package: lattice

    Attaching package: 'mice'

    The following objects are masked from 'package:base':

        cbind, rbind

    Warning in hot.deck(isq99, sdCutoff = 3, IDvars = c("IDORIGIN", "YEAR")) :
      52 observations with no observed data.  These observations were removed

    Warning in hot.deck(isq99, sdCutoff = 3, IDvars = c("IDORIGIN", "YEAR")) :
      45 of 4661 imputations with # donors < 5, consider increasing sdCutoff or using method='p.draw'

    Loading required package: survival

HSAUR3 (1.0-8)
--------------

Maintainer: Torsten Hothorn <Torsten.Hothorn@R-project.org>

0 errors | 1 warning | 0 notes

    checking re-building of vignette outputs ... WARNING
    Error in re-building vignettes:
      ...

    The following object is masked from 'package:MASS':

        geyser

    The following object is masked from 'package:HSAUR3':

    ... 8 lines ...
    Loading required package: lattice

    Attaching package: 'mice'

    The following objects are masked from 'package:base':

        cbind, rbind

    Error: processing vignette 'Ch_missing_values.Rnw' failed with diagnostics:
    subscript out of bounds
    Execution halted

JointAI (0.1.0)
---------------

Maintainer: Nicole S. Erler <n.erler@erasmusmc.nl>
Bug reports: <https://github.com/nerler/JointAI>

1 error | 0 warnings | 0 notes

    checking whether package ‘JointAI’ can be installed ... ERROR
    Installation failed.
    See ‘/Users/buurensv/Package/mice/mice/revdep/checks/JointAI.Rcheck/00install.out’ for details.

logistf (1.22)
--------------

Maintainer: Georg Heinze <georg.heinze@meduniwien.ac.at>

1 error | 0 warnings | 1 note

    checking examples ... ERROR
    Running examples in ‘logistf-Ex.R’ failed
    The error most likely occurred in:

    > base::assign(".ptime", proc.time(), pos = "CheckExEnv")
    > ### Name: pool.RR
    > ### Title: Compute Pooled Normal Confidence Intervals (following Rubin's
    > ###   Rules) after Multiple Imputation
    > ### Aliases: pool.RR
    > ### Keywords: regression models
    ... 21 lines ...
    +   toymi[[i]]$x[y1==TRUE]<-xnew1
    +   toymi[[i]]$x[y0==TRUE]<-xnew0
    + }
    > 
    > 
    > # logistf analyses of each imputed data set
    > fit.list<-lapply(1:5, function(X) logistf(data=toymi[[X]], y~x, pl=TRUE, dataout=TRUE))
    > summary(pool.RR(fit.list))
    Error in sqrt(x$t) : non-numeric argument to mathematical function
    Calls: summary -> summary.mipo
    Execution halted

    checking compiled code ... NOTE
    File ‘logistf/libs/logistf.so’:
      Found no calls to: ‘R_registerRoutines’, ‘R_useDynamicSymbols’

    It is good practice to register native routines and to disable symbol
    search.

    See ‘Writing portable packages’ in the ‘Writing R Extensions’ manual.

miceadds (2.10-14)
------------------

Maintainer: Alexander Robitzsch <robitzsch@ipn.uni-kiel.de>

1 error | 0 warnings | 0 notes

    checking examples ... ERROR
    Running examples in ‘miceadds-Ex.R’ failed
    The error most likely occurred in:

    > base::assign(".ptime", proc.time(), pos = "CheckExEnv")
    > ### Name: datlist2mids
    > ### Title: Converting a List of Multiply Imputed Data Sets into a 'mids'
    > ###   Object
    > ### Aliases: datalist2mids datlist2mids
    > ### Keywords: mids mice utility function
    ... 63 lines ...

      1  2  3  4  5  6  7  8

    > # plot of observed and imputed data
    > plot(a.out)
    > 
    > # convert list of multiply imputed datasets into a mids object
    > a.mids <- miceadds::datlist2mids( a.out$imputations )
    Error in 0 * imp1$visitSequence : non-numeric argument to binary operator
    Calls: <Anonymous>
    Execution halted

miceFast (0.2.3)
----------------

Maintainer: Maciej Nasinski <nasinski.maciej@gmail.com>
Bug reports: <https://github.com/Polkas/miceFast/issues>

1 error | 0 warnings | 0 notes

    checking whether package ‘miceFast’ can be installed ... ERROR
    Installation failed.
    See ‘/Users/buurensv/Package/mice/mice/revdep/checks/miceFast.Rcheck/00install.out’ for details.

miceMNAR (1.0)
--------------

Maintainer: Jacques-Emmanuel Galimard <jacques-emmanuel.galimard@inserm.fr>

1 error | 0 warnings | 0 notes

    checking examples ... ERROR
    Running examples in ‘miceMNAR-Ex.R’ failed
    The error most likely occurred in:

    > base::assign(".ptime", proc.time(), pos = "CheckExEnv")
    > ### Name: MNARargument
    > ### Title: Function providing modified arguments for imputation of Missing
    > ###   Not At Random (MNAR) outcomes using 'mice()' function of the "mice"
    > ###   package
    > ### Aliases: MNARargument
    ... 48 lines ...
    +                  method = arg$method,
    +                  predictorMatrix = arg$predictorMatrix,
    +                  JointModelEq=arg$JointModelEq,
    +                  control=arg$control,
    +                  maxit=1,m=5)

     iter imp variable
      1   1  hivError in model.frame.default(formula = "ry ~ ry + age + highhiv.1 + condom.1 + smoke.1 + y",  : 
      'data' must be a data.frame, not a matrix or an array
    Calls: mice ... SemiParBIV -> eval -> eval -> model.frame -> model.frame.default
    Execution halted

MissingDataGUI (0.2-5)
----------------------

Maintainer: Xiaoyue Cheng <xycheng@unomaha.edu>

1 error | 0 warnings | 0 notes

    checking package dependencies ... ERROR
    Packages required but not available:
      ‘gWidgetsRGtk2’ ‘GGally’ ‘cairoDevice’ ‘reshape’

    See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
    manual.

Qtools (1.3)
------------

Maintainer: Marco Geraci <geraci@mailbox.sc.edu>

1 error | 0 warnings | 0 notes

    checking whether package ‘Qtools’ can be installed ... ERROR
    Installation failed.
    See ‘/Users/buurensv/Package/mice/mice/revdep/checks/Qtools.Rcheck/00install.out’ for details.

rattle (5.1.0)
--------------

Maintainer: Graham Williams <Graham.Williams@togaware.com>

1 error | 0 warnings | 0 notes

    checking package dependencies ... ERROR
    Packages required but not available:
      ‘RGtk2’ ‘cairoDevice’ ‘XML’ ‘rpart.plot’

    Packages suggested but not available for checking:
      ‘pmml’ ‘ada’ ‘arules’ ‘arulesViz’ ‘biclust’ ‘cba’ ‘corrplot’ ‘descr’
      ‘doBy’ ‘fpc’ ‘ggdendro’ ‘ggraptR’ ‘gWidgetsRGtk2’ ‘hmeasure’
      ‘odfWeave’ ‘playwith’ ‘rattle.data’ ‘reshape’ ‘rggobi’ ‘RODBC’
      ‘SnowballC’ ‘tm’ ‘verification’ ‘wskm’ ‘RGtk2Extras’ ‘xgboost’

    See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
    manual.

sjmisc (2.7.2)
--------------

Maintainer: Daniel Lüdecke <d.luedecke@uke.de>
Bug reports: <https://github.com/strengejacke/sjmisc/issues>

1 error | 0 warnings | 0 notes

    checking examples ... ERROR
    Running examples in ‘sjmisc-Ex.R’ failed
    The error most likely occurred in:

    > base::assign(".ptime", proc.time(), pos = "CheckExEnv")
    > ### Name: frq
    > ### Title: Frequencies of labelled variables
    > ### Aliases: frq
    > 
    > ### ** Examples
    ... 435 lines ...
       4 63-77 216   23.79     23.97   97.11
       5 78-92  26    2.86      2.89  100.00
      NA    NA   7    0.77        NA      NA


    > 
    > # and with weights
    > efc$weights <- abs(rnorm(n = nrow(efc), mean = 1, sd = .5))
    > frq(efc, c160age, auto.grp = 5, weight.by = weights)
    Error: Package `Hmisc` needed for this function to work. Please install it.
    Execution halted

weightTAPSPACK (0.1)
--------------------

Maintainer: David G. Carlson <carlson.david@wustl.edu>

1 error | 0 warnings | 0 notes

    checking package dependencies ... ERROR
    Package required but not available: ‘HotDeckImputation’

    See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
    manual.
