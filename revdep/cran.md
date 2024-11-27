## revdepcheck results

We checked 132 reverse dependencies (127 from CRAN + 5 from Bioconductor), comparing R CMD check results across CRAN and dev versions of this package.

 * We saw 5 new problems
 * We failed to check 5 packages

Issues with CRAN packages are summarised below.

### New problems
(This reports the first line of each new failure)

* autoReg
  checking examples ... ERROR
  checking running R code from vignettes ...

* bipd
  checking dependencies in R code ...sh: line 1:  6660 Segmentation fault: 11  R_DEFAULT_PACKAGES=NULL '/Library/Frameworks/R.framework/Resources/bin/R' --vanilla --no-echo 2>&1 < '/var/folders/5_/g85d42yj50b6lrjq4rzjzg8w0000gn/T//RtmpY4Kslj/file17bf4758620f'

* finalfit
  checking examples ... ERROR
  checking running R code from vignettes ...

* gFormulaMI
  checking examples ... ERROR
  checking tests ...
  checking running R code from vignettes ...

* micd
  checking Rd cross-references ... WARNING

### Failed to check

* brms        (NA)
* dynr        (NA)
* pguIMP      (NA)
* Replication (NA)
* rmsb        (NA)
