# mice 3.16.8

* Fixes problems with zero predictors (#588)

# mice 3.16.7

### Minor changes

* Solves problem with the package documentation link
* Simplifies `NEWS.md` formatting to get correct version sequence on CRAN and in-package NEWS

# mice 3.16.6

### Minor changes

* Prepares for the deprecation of the `blocks` argument at various places
* Removes the need for `blocks` in `initialize_chain()`
* In `rbind()`, when formulas are concatenated and duplicate names are found, also rename the duplicated variables in formulas by their new name

### Bug fixes

* Fixes a bug in `filter.mids()` that incorrectly removed empty components in the `imp` object
* Fixes a bug in `ibind()` that incorrectly used `length(blocks)` as the first dimension of the `chainMean` and `chainVar` objects
* Corrects the description `visitSequence`, `chainMean` and `chainVar` components of the `mids` object

# mice 3.16.5

### Bug fixes

* Patches a bug in `complete()` that auto-repeated imputed values into cells that should NOT be imputed (occurred as a special case of `rbind()`, where the first set of rows was imputed and the second was not).
* Replaces the internal variable `type` by the more informative `pred` (currently active row of `predictorMatrix`)

# mice 3.16.4

### Major changes

* **Imputing categorical data by predictive mean matching**. Predictive mean matching (PMM) is the default method of `mice()` for imputing numerical variables, but it has long been possible to impute factors. This enhancement introduces better support to work with categorical variables in PMM. The **former system** translated factors into integers by `ynum <- as.integer(f)`. However, the order of integers in `ynum` may have no sensible interpretation for an unordered factor. The **new system** quantifies `ynum` and could yield better results because of higher $R^2$. The method calculates the canonical correlation between `y` (as dummy matrix) and a linear combination of imputation model predictors `x`. The algorithm then replaces each category of `y` by a single number taken from the first canonical variate. After this step, the imputation model is fitted, and the predicted values from that model are extracted to function as the similarity measure for the matching step.

* The method works for both ordered and unordered factors. No special precautions are taken to ensure monotonicity between the category numbers and the quantifications, so the method should be able to preserve quadratic and other non-monotone relations of the predicted metric. It may be beneficial to remove very sparsely filled categories, for which there is a new `trim` argument. All you have to use the new technique is specify to `mice(..., method = "pmm", ...)`. Both numerical and categorical variables will then be imputed by PMM.

* Potential advantages are:
  - Simpler and faster than fitting a generalised linear model, e.g., logistic regression or the proportional odds model;
  - Should be insensitive to the order of categories;
  - No need to solve problems with perfect prediction;
  - Should inherit the good statistical properties of predictive mean matching.

* Note that we still lack solid evidence for these claims. (#576). Contributed @stefvanbuuren

# mice 3.16.3

### Major changes

* **New system-independent method for pooling**: This version introduces a new function `pool.table()` that takes a tidy table of parameter estimates stemming from `m` repeated analyses. The input data must consist of three columns (parameter name, estimate, standard error) and a specification of the degrees of freedom of the model fitted to the complete data. The `pool.table()` function outputs 14 pooled statistics in a tidy form. The primary use of `pool.table()` is to support parameter pooling for techiques that have no `tidy()` or `glance()` methods, either within `R` or outside `R`. The `pool.table()` function also allows for a novel workflows that 1) break apart the traditional `pool()` function into a data-wrangling part and a parameters-reducing part, and 2) does not necessarily depend on classed R objects. (#574). Contributed @stefvanbuuren

### Bug fixes

* Fixes the "large logo" problem. (#574). Contributed @hanneoberman

# mice 3.16.2

### Major changes

* **Breaking change:** The `complete(..., action = "long", ...)` command puts the columns named `".imp"` and `".id"` in the last two positions of the long data (instead of first two positions).  In this way, the columns of the imputed data will have the same positions as in the original data, which is more user-friendly and easier to work with. Note that any existing code that assumes that variables `".imp"` and `".id"` are in columns 1 and 2 will need to be modified. The advice is to modify the code using the variable names `".imp"` and `".id"`. If you want the old behaviour, specify the argument `order = "first"`. (#569). Contributed @stefvanbuuren

# mice 3.16.1

### Minor changes

* Adds support for the `dots` argument to `ranger::ranger(...)` in `mice.impute.rf()` (#563). Contributed @edbonneville

# mice 3.16.0

### Major changes

* Expands `futuremice()` functionality by allowing for external packages and user-written functions (#550). Contributed @thomvolker

* Adds GH issue templates `bug_report`, `feature_request` and `help_wanted` (#560). Contributed @hanneoberman

### Minor changes

* Removes documentation files for `rbind.mids()` and `cbind.mids()` to conform to CRAN policy
* Adds `mitml` and `glmnet` to imports so that test code conforms to `_R_CHECK_DEPENDS_ONLY=true` flag in `R CMD check`
* Initializes random number generator in `futuremice()` if there is no `.Random.seed` yet.
* Updates GitHub actions for package checking and site building
* Preserves user settings in `predictorMatrix` for case F by adding a `predictorMatrix` argument to `make.predictorMatrix()`
* Polishes `mice.impute.mpmm()` example code

### Bug fixes

* Adds proper support for factors to `mice.impute.2lonly.pmm()` (#555)
* Solves function naming problems for S3 generic functions `tidy()`, `update()`, `format()` and `sum()`
* Out-comments and weeds example&test code to silence `R CMD check` with `_R_CHECK_DEPENDS_ONLY=true`
* Fixes small bug in `futuremice()` that throws an error when the number of cores is not specified, but the number of available cores is greater than the number of imputations.
* Solves a bug in `mice.impute.mpmm()` that changed the column order of the data

# mice 3.15.0

### Major changes

* Adds a function `futuremice()` with support for parallel imputation using the `future` package (#504). Contributed @thomvolker, @gerkovink

* Adds multivariate predictive mean matching `mice.impute.mpmm()`. (#460). Contributed @Mingyang-Cai

* Adds `convergence()` for convergence evaluation (#484). Contributed @hanneoberman

* Reverts the internal seed behaviour back to `mice 3.13.10` (#515). #432 introduced new local seed in response to #426. However, various issues arose with this facility (#459, #492, #502, #505). This version restores the old behaviour using global `.Random.seed`. Contributed @gerkovink 

* Adds a `custom.t` argument to `pool()` that allows the advanced user to specify a custom rule for calculating the total variance $T$. Contributed @gerkovink

* Adds new argument `exclude` to `mice.impute.pmm()` that excludes a user-specified vector of values from matching. Excluded values will not appear in the imputations. Since the observed values are not imputed, the user-specified values are still being used to fit the imputation model (#392, #519). Contributed @gerkovink

### Minor changes

* Styles all `.R` and `.Rmd` files
* Makes post-processing assignment consistent with lines 85/86 in `sampler.R` (#511)
* Edit test broken on R<4 (#501). Contributed @MichaelChirico
* Adds support for models reporting contrasts rather than terms (#498). Contributed @LukasWallrich
* Applies edits to autocorrelation function (#491). Contributed @hanneoberman
* Changes p-value calculation to more robust alternative (#494). Contributed @AndrewLawrence
* Uses `inherits()` to check on class membership
* Adds decprecation notices to `parlmice()`
* Adapt `prop`, `patterns` and `weights` matrices for pattern with only 1's
* Adds warning when patterns cannot be generated (#449, #317, #451)
* Adds warning on the order of model terms in `D1()` and `D2()` (#420)
* Adds example code to fit model on train data and apply to test data to `mice()`
* Adds example code on synthetic data generation and analysis in `make.where()`
* Adds testfile `test-mice.impute.rf.R`(#448)

### Bug fixes

* Replaces `.Random.seed` reads from the `.GlobalEnv` by `get(".Random.seed", envir = globalenv(), mode = "integer", inherits = FALSE)`
* Repairs capitalisation problems with `lastSeedValue` variable name
* Solves `x$lastSeedValue` problem in `cbind.mids()` (#502)
* Fixes problems with `ampute()`
* Preserves stochastic nature of `mice()` by smarter random seed initialisation (#459)
* Repairs a `drop = FALSE` buglet in `mice.impute.rf()` (#447, #448)
* @str-amg reported that the new dependency on `withr` package should have version 2.4.0  (published in January 2021) or higher. Versions `withr 2.3.0` and before may give `Error: object 'local_seed' is not exported by 'namespace:withr'`. Either update manually, or install the patched version `mice 3.14.1` from GitHub. (#445). NOTE: `withr` is no longer needed in `mice 3.15.0`

# mice 3.14.0

### Major changes

* Adds four new univariate functions using the lasso for automatic variable selection. Contributed by @EdoardoCostantini (#438). 

  - `mice.impute.lasso.norm()` for lasso linear regression
  - `mice.impute.lasso.logreg()` for lasso logistic regression
  - `mice.impute.lasso.select.norm()` for lasso selector + linear regression
  - `mice.impute.lasso.select.logreg()` for lasso selector + logistic regression

* Adds Jamshidian && Jalal's non-parametric MCAR test, `mice::MCAR()` and associated plot method. Contributed by @cjvanlissa (#423).

* Adds two new functions `pool.syn()` and `pool.scalar.syn()` that specialise pooling estimates from synthetic data. The `"reiter2003"` pooling rule assumes that synthetic data were created from complete data. Thanks Thom Volker (#436).

* By default, `mice.impute.rf()` now uses the faster `ranger` package as back-end instead of `randomForest` package. If you want the old behaviour specify the `rfPackage = "randomForest"` argument to the `mice(...)` call. Contributed @prockenschaub (#431).

### Minor changes

* Avoids changing the global `.Random.seed` (#426, #432) by implementing `withr::local_preserve_seed()` and `withr::local_seed()`. This change provides stabler behavior in complex scripts. The change does not appear to break reproducibility when `mice()` was run with a seed. Nevertheless, if you run into a reproducibility problem, install `mice 3.13.12` or before.
* Improves the imputation of parabolic data in `mice.impute.quadratic()`, adds a parameter `quad.outcome` containing the name of the outcome variable in the complete-data model. Contributed @Mingyang-Cai, @gerkovink (#408)
* Generalises `pool()` so that it processes the parameters from all `gamlss` sub-models. Thanks Marcio Augusto Diniz (#406, #405)
* Uses the robust standard error estimate for pooling when `pool()` can extract `robust.se` from the object returned by `broom::tidy()` (#310)
* Replaces URL to jstatsoft with DOI
* Update reference to literature (#442)
* Informs the user that `pool()` cannot take a `mids` object (#433)
* Updates documentation for post-processing functionality (#387)
* Adds Rcpp necessities
* Solves a problem with "last resort" initialisation of factors (#410)
* Documents the "flat-line behaviour" of `mice.impute.2l.lmer()` to indicate a problem in fitting the imputation model (#385)
* Add reprex to test (#326)
* Documents that multivariate imputation methods do not support the `post` parameter (#326)

### Bug fixes

* Contains an emergency solution as `install.on.demand()` broke the standard CRAN workflow. mice 3.14.0 does not call `install.on.demand()` anymore for recommended packages. Also, `install.on.demand()` will not run anymore in non-interactive mode.
* Repairs an error in the `mice:::barnard.rubin()` function for infinite `dfcom`. Thanks @huftis (#441).
* Solves problem with `Xi <- as.matrix(...)` in `mice.impute.2l.lmer()` that occurred when a cluster contains only one observation (#384)
* Edits the `predictorMatrix` to a monotone pattern if `visitSequence = "monotone"` and `maxit = 1` (#316)
* Solves a problem with the plot produced by `md.pattern()` (#318, #323)
* Fixes the intercept in `make.formulas()` (#305, #324)
* Fixes seed when using `newdata` in `mice.mids()` (#313, #325)
* Solves a problem with row names of the `where` element created in `rbind()` (#319)
* Solves a bug in mnar imputation routine. Contributed by Margarita Moreno Betancur.

# mice 3.13.0

### Major changes

* Updated `mids2spss()` replaces the `foreign` by `haven` package. Contributed Gerko Vink (#291)

### Minor changes

* Repairs an error in `tests\testhat\test-D1.R` that failed on `mitml 0.4-0`
* Reverts `with.mids()` function to old version because the change in commit 4634094 broke downstream package `metafor` (#292)
* Solves a glitch in `mice.impute.rf()` in finding candidate donors (#288, #289)

# mice 3.12.0

### Major changes

* **Much faster predictive mean matching**. The new `matchindex` C function makes predictive mean matching **50 to 600 times faster**. 
The speed of `pmm` is now on par with normal imputation (`mice.impute.norm()`)
and with the `miceFast` package, without compromising on the statistical quality of 
the imputations. Thanks to Polkas <https://github.com/Polkas/miceFast/issues/10> and 
suggestions by Alexander Robitzsch. See #236 for more details.

* **New `ignore` argument to `mice()`**. This argument is a logical vector 
of `nrow(data)` elements indicating which rows are ignored when creating 
the imputation model. We may use the `ignore` argument to split the data 
into a training set (on which the imputation model is built) and a test 
set (that does not influence the imputation model estimates). The argument
is based on the suggestion in 
<https://github.com/amices/mice/issues/32#issuecomment-355600365>. See #32 for 
more background and techniques. Crafted by Patrick Rockenschaub

* **New `filter()` function for `mids` objects**. New `filter()` method that 
subsets a `mids` object (multiply-imputed data set).
The method accepts a logical vector of length `nrow(data)`, or an expression
to construct such a vector from the incomplete data. (#269). 
Crafted by Patrick Rockenschaub.

* **Breaking change:** The `matcher` algorithm in `pmm` has changed to `matchindex`
for speed improvements. If you want the old behavior, specify `mice(..., use.matcher = TRUE)`.

### Minor changes

* Corrected installation problem related to `cpp11` package (#286)
* Simplifies `with.mids()` by calling `eval_tidy()` on a quosure. Does not yet solve #265.
* Improve documentation for `pool()` and `pool.scalar()` (#142, #106, #190 and others)
* Makes `tidy.mipo` more flexible (#276)
* Solves a problem if `nelsonaalen()` gets a `tibble` (#272)
* Add explanation to how `NA`s can appear in the imputed data (#267)
* Add warning to `quickpred()` documentation (#268)
* Styles all sources files with styler
* Improves consistency in code and documentation
* Moves internally defined functions to global namespace
* Solves bug in internal `sum.scores()`
* Adds deprecated messages to `lm.mids()`, `glm.mids()`, `pool.compare()`
* Removes `.pmm.match()` and `expandcov()`
* Strips out all `return()` calls placed just before end-of-function
* Remove all trailing spaces
* Repairs a bug in the routine for finding the `printFlag` value (#258)
* Update URL's after transfer to organisation `amices`

# mice 3.11.0

### Major changes

* The Cox model does not return `df.residual`, which caused problematic behavior in the `D1()`, `D2()`, `D3()`, `anova()` and `pool()`. `mice` now extracts the relevant information from other parts of the objects returned by `survival::coxph()`, which solves long-standing issues with the integration of the Cox model (#246).

### Minor changes

* Adds missing `Rccp` dependency to work with `tidyr 1.1.1` (#248).
* Addresses warnings: `Non-file package-anchored link(s) in documentation object`.
* Updates on `ampute` documentation (#251).
* Ask user permission before installing a package from `suggests`.

# mice 3.10.0

### Major changes

* New functions `tidy.mipo()` and `glance.mipo()` return standardized output that conforms to `broom` specifications. Kindly contributed by Vincent Arel Bundock (#240).

### Minor changes

* Solves a problem with the `D3` testing script that produced an error on CRAN (#244).

# mice 3.9.0

### Major changes

* The `D3()` function in `mice` gave incorrect results. This version solves a problem in the calculation of the `D3`-statistic. See #226 and #228 for more details. The documentation explains why results from `mice::D3()` and `mitml::testModels()` may differ.

* The `pool()` function is now more forgiving when there is no `glance()` function (#233)

* It is possible to bypass `remove.lindep()` by setting `eps = 0` (#225)

### Minor changes

* Adds reference to Leacy's thesis
* Adds an example to the `plot.mids()` documentation

# mice 3.8.0

### Major changes 

* This version adds two new NARFCS methods for imputing data under the *Missing Not at Random (MNAR)* assumption. NARFCS is generalised version of the so-called $\delta$-adjustment method. Margarita Moreno-Betancur and Ian White kindly contributes the functions `mice.impute.mnar.norm()` and `mice.impute.mnar.logreg()`. These functions aid in performing sensitivity analysis to investigate the impact of different MNAR assumptions on the conclusion of the study. An alternative for MNAR is the older `mice.impute.ri()` function.

* Installation of `mice` is faster. External packages needed for imputation and analyses are now installed on demand. The number of dependencies as estimated by `rsconnect::appDepencies()` decreased from 132 to 83.

* The name clash with the `complete()` function of `tidyr` should no longer be a problem.

* There is now a more flexible `pool()` function that integrates better with the `broom` and `broom.mixed` packages.

### Bug fixes

* Deprecates `pool.compare()`. Use `D1()` instead (#220)
* Removes everything in `utils::globalVariables()`
* Prevents name clashes with `tidyr` by defining `complete.mids()` as an S3 method for the `tidyr::complete()` generic (#212)
* Extends the `pool()` function to deal with multiple sets of parameters. Currently supported keywords are: `term` (all `broom` functions), `component` (some `broom.mixed` functions) and `y.values` (for `multinom()` model) (#219)
* Adds a new `install.on.demand()` function for lighter installation
* Adds `toenail2` and remove dependency on `HSAUR3`
* Solves problem with `ampute` in extreme cases (#216)
* Solves problem with `pool` with `mgcv::gam` (#218)
* Adds `.gitattributes` for consistent line endings

# mice 3.7.0

* Solves a bug that made `polr()` always fail (#206)
* Aborts if one or more columns are a `data.frame` (#208)
* Update `mira-class` documentation (#207)
* Remove links to deprecated package `CALIBERrfimpute`
* Adds check on partial missing level-2 data to `2lonly.norm` and `2lonly.pmm`
* Change calculation of `a2` to elementwise division by a matrix of observations
* Extend documentation for `2lonly.norm` and `2lonly.pmm`
* Repair return value from `2lonly.pmm`
* Imputation method `2lonly.mean` now also works with factors
* Replace deprecated `imputationMethod` argument in examples by `method`
* More informative error message when stopped after pre-processing (#194)
* Updated URL's in DESCRIPTION
* Fix string matching in `check.predictorMatrix()` (#191)

# mice 3.6.0

* Copy `toenail` data from orphaned `DPpackage` package
* Remove `DPpackage`  from `Suggests` field in `DESCRIPTION` 
* Adds support for rotated names in `md.pattern()` (#170, #177)

# mice 3.5.0

* This version has some error fixes
* Fixes a bug in the sampler that ignored imputed values in variables outside the active block (#175, @alexanderrobitzsch)
* Adds a note to the documenation of `as.mids`() (#173)
* Removes a superfluous warning from process_mipo() (#92)
* Fixes an error in the degrees of freedom of the P-value calculation (#171)

# mice 3.4.0 

* Add a hex sticker to the mice package. Designed by Jaden M. Walters.
* Specify the R3.5.0 random generator in order to pass CRAN tests
* Remove test-fix.coef.R from tests
* Adds a rotate.names argument to md.pattern() (#154, #160)
* Fix to solve the name-matching problem (#156, #149, #147)
* Fix that removes the pre-check for existence of `mice.impute.xxx()` so that `mice::mice()` works as expected (#55)
* Solves a bug that crashed `mids2spss()`, thanks Edgar Schoreit (#149)
* Solves a problem in the routing logic (#149) causing that passive 
imputation was not done when no predictors were specified. No passive
imputation correctly will ignore any the specification of 
`predictorMatrix`.
* Implements an alternative solution for #93 and #96. Instead of skipping 
imputation of variables without predictors, `mice 3.3.1` will impute 
those variables using the intercept only
* Adds a routine contributed by Simon Grund that checks for deprecated 
arguments #137
* Improves the `nelsonaalen()` function for data where variables 
`time` or `status` have already been defined (#140), thanks matthieu-faron

# mice 3.3.0

* Solves bug in passive imputation (#130). *Warning: This bug may 
have caused invalid imputations in `mice 3.0.0` - `mice 3.2.0` under 
passive imputation.*
* Updates code to `broom 0.5.0` (#128)
* Solves problem with `mice.impute.2l.norm()` (#129)
* Use explicit foreign function calls in tests

# mice 3.2.0

* Skip tests for `mice.impute.2l.norm()` (#129)
* Skip tests for `D1()` (#128)
* Solve problem with `md.pattern` (#126)
* Evades warning in `rbind` and `cbind` (#114)
* Solves `rbind` problem when `method` is a list (#113)
* More efficient use of `parlmice` (#109)
* Add `dfcom` argument to `pool()` (#105, #110)
* Updates to `parlmice` + bugfix (#107)

# mice 3.1.0

* New parallel functionality: `parlmice` (#104)
* Incorporate suggestion of @JoergMBeyer to `flux` (#102)
* Replace duplicate code by `estimice` (#101)
* Better checking for empty methods (#99)
* Remove problem with `parent.frame` (#98)
* Set empty method for complete data (#93)
* Add `NEWS.md`, `index.Rmd` and online package documentation
* Track `.R` instead of `.r`
* Patch issue with `updateLog` (#8, @alexanderrobitzsch)
* Extend README
* Repair issue `md.pattern` (#90)
* Repair check on `m` (#89)

# mice 3.0.0 
         
Version 3.0 represents a major update that implements the 
following features: 

1. `blocks`: The main algorithm iterates over blocks. A block is
    simply a collection of variables. In the common MICE algorithm each 
    block was equivalent to one variable, which - of course - is 
    the default; The `blocks` argument allows mixing univariate 
    imputation method multivariate imputation methods. The `blocks` 
    feature bridges two seemingly disparate approaches, joint modeling 
    and fully conditional specification, into one framework;

2. `where`: The `where` argument is a logical matrix of the same size 
    of `data` that specifies which cells should be imputed. This opens 
    up some new analytic possibilities;
    
3.  Multivariate tests: There are new functions `D1()`, `D2()`, `D3()`
    and `anova()` that perform multivariate parameter tests on the 
    repeated analysis from on multiply-imputed data;

4. `formulas`: The old `form` argument has been redesign and is now 
    renamed to `formulas`. This provides an alternative way to specify
    imputation models that exploits the full power of R's native 
    formula's. 

5.  Better integration with the `tidyverse` framework, especially 
    for packages `dplyr`, `tibble` and  `broom`;
   
6.  Improved numerical algorithms for low-level imputation function. 
    Better handling of duplicate variables.

7.  Last but not least: A brand new edition AND online version of
    [Flexible Imputation of Missing Data. Second Edition.](https://stefvanbuuren.name/fimd/)


# mice 2.46.9

* simplify code for `mids` object in `mice` (thanks stephematician) (#61)
* simplify code in `rbind.mids` (thanks stephematician) (#59)
* repair bug in `pool.compare()` in handling factors (#60)
* fixed bug in `rbind.mids` in handling `where` (#59)
* add new arguments to `as.mids()`, add `as()`
* update contact info
* resolved problem `cart` not accepting a matrix (thanks Joerg Drechsler)
* Adds generalized `pool()` to list of models
* Switch to 3-digit versioning
* Date: 2017-12-08

# mice 2.46

* Allow for capitals in imputation methods
* Date: 2017-10-22

# mice 2.45

* Reorganized vignettes to land on GitHUB pages
* Date: 2017-10-21

# mice 2.44

* Code changes for robustness, style and efficiency (Bernie Gray)
* Date: 2017-10-18

# mice 2.43

* Updates the `ampute` function and vignettes (Rianne Schouten)
* Date: 2017-07-20

# mice 2.42

* Rename `mice.impute.2l.sys` to `mice.impute.2l.lmer`
* Date: 2017-07-11

# mice 2.41

* Add new feature: `where`argument to mice
* Add new `wy` argument to imputation functions
* Add `mice.impute.2l.sys()`, author Shahab Jolani
* Update with many simplifications and code enhancements
* Fixed broken `cbind()` function
* Fixed Bug that made the pad element disappear from `mids` object
* Date: 2017-07-10

# mice 2.40

* Fixed integration with `lattice` package
* Updates colors in `xyplot.mads`
* Add support for factors in `mice.impute.2lonly.pmm()`
* Create more robust version of as.mids()
* Update of `ampute()` by Rianne Schouten
* Fix timestamp problem by rebuilding vignette using R 3.4.0.
* Date: 2017-07-07

# mice 2.34

* Update to roxygen 6.0.1
* Stylistic changes to `mice` function (thanks Ben Ogorek)
* Calls to `cbind.mids()` replaced by calls to `cbind()`
* Date:  2017-04-24

# mice 2.31

* Add link to `miceVignettes` on github (thanks Gerko Vink)
* Add package documentation
* Add `README` for GitHub
* Add new ampute functions and vignette (thanks Rianne Schouten)
* Rename `ccn` --> `ncc`, `icn` --> `nic`
* Change helpers `cc()`, `ncc()`, `cci()`, `ic()`, `nic()` and `ici()` use `S3` dispatch
* Change issues tracker on Github - add BugReports URL #21
* Fixed `multinom` MaxNWts type fix in `polyreg` and `polr` #9
* Fix checking of nested models in `pool.compare` #12
* Fix `as.mids` if names not same as all columns #11
* Fix extension for `glmer` models #5
* Date: 2017-02-23

# mice 2.29

* Add `midastouch`: predictive mean matching for small samples (thanks Philip Gaffert, Florian Meinfelder)
* Date: 2016-10-05

# mice 2.28

* Repaired dots problem in `rpart` call
* Date: 2016-10-05

# mice 2.27

* Add `ridge` to `2l.norm()`
* Remove `.o` files
* Date: 2016-07-27

# mice 2.25

* Fix `as.mids()` bug that crashed `miceadds::mice.1chain()`
* Date: 2015-11-09

# mice 2.23

* Update of example code on /doc
* Remove lots of dependencies, general cleanup 
* Fix `impute.polyreg()` bug that bombed if there were no predictors (thanks Jan Graffelman)
* Fix `as.mids()` bug that gave incorrect $m$ (several users)
* Fix `pool.compare()` error for `lmer` object (thanks Claudio Bustos)
* Fix error in `mice.impute.2l.norm()` if just one `NA` (thanks Jeroen Hoogland)
* Date: 2015-11-04

# mice 2.22

* Add about six times faster predictive mean matching
* `pool.scalar()` now can do Barnard-Rubin adjustment
* `pool()` now handles class `lmerMod` from the `lme4` package
* Added automatic bounds on donors in `.pmm.match()` for safety
* Added donors argument to `mice.impute.pmm()` for increased visibility
* Changes default number of trees in `mice.impute.rf()` from 100 to 10 (thanks Anoop Shah) 
* `long2mids()` deprecated. Use `as.mids()` instead
* Put `lattice` back into DEPENDS to find generic `xyplot()` and friends
* Fix error in `2lonly.pmm` (thanks Alexander Robitzsch, Gerko Vink, Judith Godin)
* Fix number of imputations in `as.mids()` (thanks Tommy Nyberg, Gerko Vink)
* Fix colors to `mdc()` in example `mice.impute.quadratic()`
* Fix error in `mice.impute.rf()` if just one `NA` (thanks Anoop Shah)
* Fix error in `summary.mipo()` when `names(x$qbar)` equals `NULL` (thanks Aiko Kuhn)
* Fix improper testing in `ncol()` in `mice.impute.2lonly.mean()` 
* Date: 2014-06-11

# mice 2.21

* FIXED:     compilation problem in match.cpp on solaris CC 
* Date: 02-05-2014 SvB

# mice 2.20

* ADDED:     experimental fastpmm() function using Rcpp
* FIXED:     fixes to mice.impute.cart() and mice.impute.rf() (thanks Anoop Shah)
* Date: 02-02-2014 SvB

# mice 2.19

* ADDED:     mice.impute.rf() for random forest imputation (thanks Lisa Doove)
* CHANGED:  default number of donors in mice.impute.pmm() changed from 3 to 5.
         Use mice(..., donors = 3) to get the old behavior.
* CHANGED:  speedup in .norm.draw() by using crossprod() (thanks Alexander Robitzsch)
* CHANGED:  speedup in .imputation.level2() (thanks Alexander Robitzsch)
* FIXED:     define MASS, nnet, lattice as imports instead of depends
* FIXED:     proper handling of rare case in remove.lindep() that removed all predictors (thanks Jaap Brand)
* Date: 21-01-2014 SvB

# mice 2.18

* ADDED:     as.mids() for converting long format in a mids object (thanks Gerko Vink)
* FIXED:     mice.impute.logreg.boot() now properly exported (thanks Suresh Pujar)
* FIXED:     two bugs in rbind.mids() (thanks Gerko Vink)
* Date: 31-07-2013 SvB

# mice 2.17

* ADDED:     new form argument to mice() to specify imputation models using forms (contributed Ross Boylan)
* FIXED:     with.mids(), is.mids(), is.mira() and is.mipo() exported
* FIXED:     eliminated errors in the documentation of pool.scalar()
* FIXED:     error in mice.impute.ri() (thanks Shahab Jolani)
* Date: 10-05-2013 SvB

# mice 2.16

* ADDED:     random indicator imputation by mice.impute.ri() for nonignorable models (thanks Shahab Jolani)
* ADDED:     workhorse functions .norm.draw() and .pmm.match() are exported
* FIXED:     bug in 2.14 and 2.15 in mice.impute.pmm() that produced an error on factors
* FIXED:     bug that crashed R when the class variable was incomplete (thanks Robert Long)
* FIXED:     bug in 2l.pan and 2l.norm by convert a class factor to integer (thanks Robert Long)
* FIXED:     warning eliminated caused by character variables (thanks Robert Long)
* Date: 27-04-2013 SvB

# mice 2.15

* CHANGED:  complete reorganization of documentation and source files
* ADDED:     source published on GitHub.com
* ADDED:     new imputation method mice.impute.cart() (thanks Lisa Doove)
* FIXED:     calculation of degrees of freedom in pool.compare() (thanks Lorenz Uhlmann)
* FIXED:     error in DESCRIPTION file (thanks Kurt Hornik)
* Date: 02-04-2013 SvB

# mice 2.14

* ADDED:     mice.impute.2l.mean() for imputing class means at level 2
* ADDED:     sampler(): new checks of degrees of freedom per variable at iteration 1
* ADDED:     function check.df() to throw a warning about low degrees of freedom
* FIXED:     tolower() added in "2l" test in sampler()
* FIXED:     conversion of factors that have other roles (multilevel) in padModel()
* FIXED:     family argument in call to glm() in glm.mids() (thanks Nicholas Horton)
* FIXED:     .norm.draw(): evading NaN imputed values by setting df in rchisq() to a minimum of 1
* FIXED:     bug in mice.df() that prevented the classic Rubin df calculation (thanks Jean-Batiste Pingaul)
* FIXED:     bug fixed in mice.impute.2l.norm() (thanks Robert Long)
* CHANGED:  faster .pmm.match2() from version 2.12 renamed to default .pmm.match()
* Date: 11-03-2013 / SvB

# mice 2.13

* ADDED:     new multilevel functions 2l.pan(), 2lonly.norm(), 2lonly.pmm() (contributed by Alexander Robitzsch)
* ADDED:     new quadratic imputation function: quadratic() (contributed by Gerko Vink)
* ADDED:     pmm2(), five times faster than pmm()
* ADDED:     new argument data.init in mice() for initialization (suggested by Alexander Robitzsch)
* ADDED:     mice() now accepts pmm as method for (ordered) factors
* ADDED:     warning and a note to 2l.norm() that advises to use type=2 for the predictors
* FIXED:     bug that chrashed plot.mids() if there was only one incomplete variable (thanks Dennis Prangle)
* FIXED:     bug in sample() in .pmm.match() when donor=1 (thanks Alexander Robitzsch)
* FIXED:     bug in sample() in mice.impute.sample()
* FIXED:     fixed '?data' bug in check.method()
* REMOVED: 	 wp.twin(). Now available from the AGD package
* Date: 03-07-2012 / SvB

# mice 2.12

* UPDATE:    version for launch of Flexible Imputation of Missing Data (FIMD)
* ADDED:     code fimd1.r-fim9.r to inst/doc for calculating solutions in FIMD
* FIXED:     more robust version of supports.transparent() (thanks Brian Ripley)
* ADDED:     auxiliary functions ifdo(), long2mids(), appendbreak(), extractBS(), wp.twin()
* ADDED:     getfit() function
* ADDED:     datasets: tbc, potthoffroy, selfreport, walking, fdd, fdgs, pattern1-pattern4, mammalsleep
* FIXED:     as.mira() added to namespace
* ADDED:  	 functions flux(), fluxplot() and fico() for missing data patterns
* ADDED:     function nelsonaalen() for imputing survival data
* CHANGED:  rm.whitespace() shortened
* FIXED:     bug in pool() that crashed on nonstandard behavior of survreg() (thanks Erich Studerus)
* CHANGED:  pool() streamlined, warnings about incompatibility in lengths of coef() and vcov()
* FIXED:     mdc() bug that ignored transparent=FALSE argument, now made visible
* FIXED:     bug in md.pattern() for >32 variables (thanks Sascha Vieweg, Joshua Wiley)
* Date: 25-03-2012 / SvB

# mice 2.11

* UPDATE: definite reference to JSS paper
* ADDED:     rm.whitespace() to do string manipulation (thanks Gerko Vink)
* ADDED:     function mids2mplus() to export data to Mplus (thanks Gerko Vink)
* CHANGED:  plot.mids() changed into trellis version
* ADDED:     code used in JSS-paper
* FIXED:     bug in check.method() (thanks Gerko Vink)
* Date: 21-11-2011 / SvB

# mice 2.10

* FIXED:  arguments dec and sep in mids2spss (thanks Nicole Haag)
* FIXED:  bug in keyword "monotone" in mice() (thanks Alain D)
* Date: 14-09-2011 / SvB

# mice 2.9

* FIXED:   appropriate trimming of ynames and xnames in Trellis plots
* FIXED:   exported: spss2mids(), mice.impute.2L.norm()
* ADDED:   mice.impute.norm.predict(), mice.impute.norm.boot(), mice.impute.logreg.boot()
* ADDED:   supports.transparent() to detect whether .Device can do semi-transparent colors
* FIXED:   stringr package is now properly loaded
* ADDED:   trellis version of plot.mids()
* ADDED:   automatic semi-transparancy detection in mdc()
* FIXED:   documentation of mira class (thanks Sandro Tsang)
* Date: 31-08-2011 / SvB

# mice 2.8

* FIXED:   bug fixed in find.collinear() that bombed when only one variable was left
* Date: 24-03-2011 / SvB

# mice 2.7

* CHANGED: check.data(), remove.lindep(): fully missing variables are imputed if allow.na=TRUE (Alexander Robitzsch)
* FIXED:   bug in check.data(). Now checks collinearity in predictors only (Alexander Robitzsch)
* CHANGED: abbreviations of arguments eliminated to evade linux warnings
* Date: 16-03-2011 / SvB

# mice 2.6

* ADDED:   bwplot(), stripplot(), densityplot() and xyplot() for creating Trellis graphs
* ADDED:   function mdc() and mice.theme() for graphical parameters
* ADDED:   argument passing from mice() to lower-level functions (requested by Juned Siddique)
* FIXED:   erroneous rgamma() replaced by rchisq() in .norm.draw, lowers variance a bit for small n
* ADDED:   with.mids() extended to handle expression objects
* FIXED:   reporting bug in summary.mipo()
* CHANGED: df calculation in pool(), intervals may become slightly wider
* ADDED:   internal functions mice.df() and df.residual()
* FIXED:   error in rm calculation for "likelihood" in pool.compare()
* CHANGED: default ridge parameter changed
* Date: 03-03-2011 / SvB

# mice 2.5

* ADDED:   various stability enhancements and code clean-up
* ADDED:   find.collinear() function
* CHANGED: automatic removal of constant and collinear variables
* ADDED:   ridge parameter in .norm.draw() and .norm.fix()
* ADDED:   mice.impute.polr() for ordered factors
* FIXED:   chainMean and chainVar in mice.mids()
* FIXED:   iteration counter for mice.mids and sampler()
* ADDED:   component 'loggedEvents' to mids-object for logging actions
* REMOVED: annoying warnings about removed predictors
* ADDED:   updateLog() function
* CHANGED: smarter handling of model setup in mice()
* CHANGED: .pmm.match() now draws from the three closest donors
* ADDED:   mids2spss() for shipping a mids-object to SPSS
* FIXED:   change in summary.mipo() to work with as.mira()
* ADDED:   function mice.impute.2L.norm.noint()
* ADDED:   function as.mira()
* FIXED:   global assign() removed from mice.impute.polyreg()
* FIXED:   improved handling of factors by complete()
* FIXED:   improved labeling of nhanes2 data
* Date: 06-01-2011 / SvB

# mice 2.4

* ADDED:   pool() now supports class 'polr' (Jean-Baptiste Pingault)
* FIXED:   solved problem in mice.impute.polyreg when one of the variables was named y or x
* FIXED:   remove.lindep: intercept prediction bug
* ADDED:   version() function
* ADDED:   cc(), cci() and ccn() convenience functions
* Date: 17-10-2010 / SvB

# mice 2.3

* FIXED:   check.method: logicals are now treated as binary variables (Emmanuel Charpentier)
* FIXED:   complete: the NULL imputation case is now properly handled
* FIXED:   mice.impute.pmm: now creates between imputation variability for univariate predictor
* FIXED:   remove.lindep: returns 'keep' vector instead of data
* Date:    14-02-2010 / SvB

# mice 2.2

* ADDED:   pool() now supports class 'multinom' (Jean-Baptiste Pingault)
* FIXED:   bug fixed in check.data for data consisting of two columns (Rogier Donders, Thomas Koepsell)
* ADDED:   new function remove.lindep() that removes predictors that are (almost) linearly dependent
* FIXED:   bug fixed in pool() that produced an (innocent) warning message (Qi Zheng)
* Date:    13-01-2010 / SvB

# mice 2.1

* ADDED:   pool() now also supports class 'mer'
* CHANGED: nlme and lme4 are now only loaded if needed (by pool())
* FIXED:   bug fixed in mice.impute.polyreg() when there was one missing entry (Emmanuel Charpentier)
* FIXED:   bug fixed in plot.mids() when there was one missing entry (Emmanuel Charpentier)
* CHANGED: NAMESPACE expanded to allow easy access to function code
* FIXED:   mice() can now find mice.impute.xxx() functions in the .GlobalEnv
* Date:  14-09-2009 / SvB

# mice 2.0

* Major upgrade for JSS manuscript
* ADDED:   new functions cbind.mids(), rbind.mids(), ibind()
* ADDED:   new argument in mice(): 'post' in post-processing imputations
* ADDED:   new functions: pool.scaler(), pool.compare(), pool.r.squared()
* ADDED:	 new data: boys, popmis, windspeed
* FIXED:	 function summary.mipo all(object$df) command fixed
* REMOVED: data.frame.to.matrix replaced by the internal data.matrix function
* ADDED:   new imputation method mice.impute.2l.norm() for multilevel data
* CHANGED: pool now works for any class having a vcov() method
* ADDED:   with.mids() provides a general complete-data analysis
* ADDED:   type checking in mice() to ensure appropriate imputation methods
* ADDED:   warning added in mice() for constant predictors
* ADDED:   prevention of perfect prediction in mice.impute.logreg() and mice.impute.polyreg()
* CHANGED: mice.impute.norm.improper() changed into mice.impute.norm.nob()
* REMOVED: mice.impute.polyreg2() deleted
* ADDED:    new 'include' argument in complete()
* ADDED:    support for the empty imputation method in mice()
* ADDED:    new function md.pairs()
* ADDED:    support for intercept imputation
* ADDED:    new function quickpred()
* FIXED:   plot.mids() bug fix when number of variables > 5
* Date:  26-08-2009 / SvB, KO 	

# mice 1.21

* FIXED:   Stricter type checking on logicals in mice() to evade warnings.
* CHANGED: Modernization of all help files.
* FIXED:   padModel: treatment changed to contr.treatment
* CHANGED: Functions check.visitSequence, check.predictorMatrix, check.imputationMethod are now coded as local to mice()
* FIXED:   existsFunction in check.imputationMethod now works both under S-Plus and R
* Date: 15/3/2009

# mice 1.16

* FIXED: The impution function impute.logreg used convergence criteria that were too optimistic when fitting a GLM with glm.fit. Thanks to Ulrike Gromping.
* Date: 6/25/2007

# mice 1.15

* FIXED: In the lm.mids and glm.mids functions, parameters were not passed through to glm and lm.
* Date: 01/09/2006

# mice 1.14

* FIXED: Passive imputation works again. (Roel de Jong)
* CHANGED: Random seed is now left alone, UNLESS the argument "seed" is specified. This means that unless you
specify identical seed values, imputations of the same dataset will be different for multiple calls to mice. (Roel de Jong)
* FIXED:  (docs): Documentation for "impute.mean" (Roel de Jong)
* FIXED: Function 'summary.mids' now works (Roel de Jong)
* FIXED: Imputation function 'impute.polyreg' and 'impute.lda' should now work under R
* Date: 9/26/2005

# mice 1.13

* Changed function checkImputationMethod
* Date: Feb 6, 2004

# mice 1.12

* Maintainance, S-Plus 6.1 and R 1.8 unicode
* Date: January 2004

# mice 1.1

* R version (with help of Peter Malewski and Frank Harrell)
* Date: Feb 2001

# mice 1.0

* Original S-PLUS release
* Date: June 14 2000