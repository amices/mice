## revdepcheck results

We checked 177 reverse dependencies (171 from CRAN + 6 from Bioconductor), comparing R CMD check results across CRAN and dev versions of this package.

 * We saw 5 new problems
 * We failed to check 4 packages

Issues with CRAN packages are summarised below.

### New problems
(This reports the first line of each new failure)

* autoReg
  checking examples ... ERROR
  checking running R code from vignettes ...

* bipd
  checking dependencies in R code ...sh: line 1: 13687 Segmentation fault: 11  R_DEFAULT_PACKAGES=NULL '/Library/Frameworks/R.framework/Resources/bin/R' --vanilla --no-echo 2>&1 < '/var/folders/5_/g85d42yj50b6lrjq4rzjzg8w0000gn/T//RtmpM2y2Bj/file323367539f29'

* broom.mixed
  checking examples ...sh: line 1: 24508 Segmentation fault: 11  LANGUAGE=en _R_CHECK_INTERNALS2_=1 '/Library/Frameworks/R.framework/Resources/bin/R' --vanilla > 'broom.mixed-Ex.Rout' 2>&1 < 'broom.mixed-Ex.R'

* pminternal
  checking running R code from vignettes ...

* pre
  checking tests ...

### Failed to check

* brms   (NA)
* dynr   (NA)
* pguIMP (NA)
* rmsb   (NA)
