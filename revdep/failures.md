# BGGM

<details>

* Version: 2.1.5
* GitHub: https://github.com/donaldRwilliams/BGGM
* Source code: https://github.com/cran/BGGM
* Date/Publication: 2024-12-22 21:40:02 UTC
* Number of recursive dependencies: 210

Run `revdepcheck::revdep_details(, "BGGM")` for more info

</details>

## In both

*   checking whether package ‘BGGM’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/buurensv/Dropbox/Package/mice/mice/revdep/checks.noindex/BGGM/new/BGGM.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘BGGM’ ...
** this is package ‘BGGM’ version ‘2.1.5’
** package ‘BGGM’ successfully unpacked and MD5 sums checked
** using staged installation
configure: creating ./config.status
config.status: creating src/Makevars
** libs
using C++ compiler: ‘Apple clang version 17.0.0 (clang-1700.0.13.3)’
using C++17
using SDK: ‘’
clang++ -arch arm64 -std=gnu++17 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -DARMA_NO_DEBUG -I'/Users/buurensv/Dropbox/Package/mice/mice/revdep/library.noindex/mice/new/Rcpp/include' -I'/Users/buurensv/Dropbox/Package/mice/mice/revdep/library.noindex/BGGM/RcppArmadillo/include' -I'/Users/buurensv/Dropbox/Package/mice/mice/revdep/library.noindex/BGGM/RcppDist/include' -I'/Users/buurensv/Dropbox/Package/mice/mice/revdep/library.noindex/BGGM/RcppProgress/include' -I/opt/R/arm64/include -I/opt/homebrew/include   -I../inst/include -fPIC  -falign-functions=64 -Wall -g -O2   -c RcppExports.cpp -o RcppExports.o
clang++ -arch arm64 -std=gnu++17 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -DARMA_NO_DEBUG -I'/Users/buurensv/Dropbox/Package/mice/mice/revdep/library.noindex/mice/new/Rcpp/include' -I'/Users/buurensv/Dropbox/Package/mice/mice/revdep/library.noindex/BGGM/RcppArmadillo/include' -I'/Users/buurensv/Dropbox/Package/mice/mice/revdep/library.noindex/BGGM/RcppDist/include' -I'/Users/buurensv/Dropbox/Package/mice/mice/revdep/library.noindex/BGGM/RcppProgress/include' -I/opt/R/arm64/include -I/opt/homebrew/include   -I../inst/include -fPIC  -falign-functions=64 -Wall -g -O2   -c bggm_fast.cpp -o bggm_fast.o
clang++ -arch arm64 -std=gnu++17 -dynamiclib -Wl,-headerpad_max_install_names -undefined dynamic_lookup -L/Library/Frameworks/R.framework/Resources/lib -L/opt/R/arm64/lib -L/opt/homebrew/lib -o BGGM.so RcppExports.o bggm_fast.o -L/Library/Frameworks/R.framework/Resources/lib -lRlapack -L/Library/Frameworks/R.framework/Resources/lib -lRblas -L/opt/gfortran/lib/gcc/aarch64-apple-darwin20.0/14.2.0 -L/opt/gfortran/lib -lemutls_w -lheapt_w -lgfortran -lquadmath -F/Library/Frameworks/R.framework/.. -framework R
ld: warning: search path '/opt/gfortran/lib/gcc/aarch64-apple-darwin20.0/14.2.0' not found
ld: library 'emutls_w' not found
clang++: error: linker command failed with exit code 1 (use -v to see invocation)
make: *** [BGGM.so] Error 1
ERROR: compilation failed for package ‘BGGM’
* removing ‘/Users/buurensv/Dropbox/Package/mice/mice/revdep/checks.noindex/BGGM/new/BGGM.Rcheck/BGGM’


```
### CRAN

```
* installing *source* package ‘BGGM’ ...
** this is package ‘BGGM’ version ‘2.1.5’
** package ‘BGGM’ successfully unpacked and MD5 sums checked
** using staged installation
configure: creating ./config.status
config.status: creating src/Makevars
** libs
using C++ compiler: ‘Apple clang version 17.0.0 (clang-1700.0.13.3)’
using C++17
using SDK: ‘’
clang++ -arch arm64 -std=gnu++17 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -DARMA_NO_DEBUG -I'/Users/buurensv/Dropbox/Package/mice/mice/revdep/library.noindex/mice/old/Rcpp/include' -I'/Users/buurensv/Dropbox/Package/mice/mice/revdep/library.noindex/BGGM/RcppArmadillo/include' -I'/Users/buurensv/Dropbox/Package/mice/mice/revdep/library.noindex/BGGM/RcppDist/include' -I'/Users/buurensv/Dropbox/Package/mice/mice/revdep/library.noindex/BGGM/RcppProgress/include' -I/opt/R/arm64/include -I/opt/homebrew/include   -I../inst/include -fPIC  -falign-functions=64 -Wall -g -O2   -c RcppExports.cpp -o RcppExports.o
clang++ -arch arm64 -std=gnu++17 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -DARMA_NO_DEBUG -I'/Users/buurensv/Dropbox/Package/mice/mice/revdep/library.noindex/mice/old/Rcpp/include' -I'/Users/buurensv/Dropbox/Package/mice/mice/revdep/library.noindex/BGGM/RcppArmadillo/include' -I'/Users/buurensv/Dropbox/Package/mice/mice/revdep/library.noindex/BGGM/RcppDist/include' -I'/Users/buurensv/Dropbox/Package/mice/mice/revdep/library.noindex/BGGM/RcppProgress/include' -I/opt/R/arm64/include -I/opt/homebrew/include   -I../inst/include -fPIC  -falign-functions=64 -Wall -g -O2   -c bggm_fast.cpp -o bggm_fast.o
clang++ -arch arm64 -std=gnu++17 -dynamiclib -Wl,-headerpad_max_install_names -undefined dynamic_lookup -L/Library/Frameworks/R.framework/Resources/lib -L/opt/R/arm64/lib -L/opt/homebrew/lib -o BGGM.so RcppExports.o bggm_fast.o -L/Library/Frameworks/R.framework/Resources/lib -lRlapack -L/Library/Frameworks/R.framework/Resources/lib -lRblas -L/opt/gfortran/lib/gcc/aarch64-apple-darwin20.0/14.2.0 -L/opt/gfortran/lib -lemutls_w -lheapt_w -lgfortran -lquadmath -F/Library/Frameworks/R.framework/.. -framework R
ld: warning: search path '/opt/gfortran/lib/gcc/aarch64-apple-darwin20.0/14.2.0' not found
ld: library 'emutls_w' not found
clang++: error: linker command failed with exit code 1 (use -v to see invocation)
make: *** [BGGM.so] Error 1
ERROR: compilation failed for package ‘BGGM’
* removing ‘/Users/buurensv/Dropbox/Package/mice/mice/revdep/checks.noindex/BGGM/old/BGGM.Rcheck/BGGM’


```
# brms

<details>

* Version: 2.22.0
* GitHub: https://github.com/paul-buerkner/brms
* Source code: https://github.com/cran/brms
* Date/Publication: 2024-09-23 13:00:29 UTC
* Number of recursive dependencies: 205

Run `revdepcheck::revdep_details(, "brms")` for more info

</details>

## In both

*   R CMD check timed out
    

*   checking whether package ‘brms’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: namespace ‘colorspace’ is not available and has been replaced
    See ‘/Users/buurensv/Dropbox/Package/mice/mice/revdep/checks.noindex/brms/new/brms.Rcheck/00install.out’ for details.
    ```

# clusterMI

<details>

* Version: 1.5
* GitHub: NA
* Source code: https://github.com/cran/clusterMI
* Date/Publication: 2025-02-24 17:20:08 UTC
* Number of recursive dependencies: 240

Run `revdepcheck::revdep_details(, "clusterMI")` for more info

</details>

## In both

*   checking whether package ‘clusterMI’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/buurensv/Dropbox/Package/mice/mice/revdep/checks.noindex/clusterMI/new/clusterMI.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘clusterMI’ ...
** this is package ‘clusterMI’ version ‘1.5’
** package ‘clusterMI’ successfully unpacked and MD5 sums checked
** using staged installation
** libs
using C++ compiler: ‘Apple clang version 17.0.0 (clang-1700.0.13.3)’
using SDK: ‘’
clang++ -arch arm64 -std=gnu++17 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I'/Users/buurensv/Dropbox/Package/mice/mice/revdep/library.noindex/mice/new/Rcpp/include' -I'/Users/buurensv/Dropbox/Package/mice/mice/revdep/library.noindex/clusterMI/RcppArmadillo/include' -I/opt/R/arm64/include -I/opt/homebrew/include    -fPIC  -falign-functions=64 -Wall -g -O2   -c 1_Main.cpp -o 1_Main.o
clang++ -arch arm64 -std=gnu++17 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I'/Users/buurensv/Dropbox/Package/mice/mice/revdep/library.noindex/mice/new/Rcpp/include' -I'/Users/buurensv/Dropbox/Package/mice/mice/revdep/library.noindex/clusterMI/RcppArmadillo/include' -I/opt/R/arm64/include -I/opt/homebrew/include    -fPIC  -falign-functions=64 -Wall -g -O2   -c 2_DataParam.cpp -o 2_DataParam.o
clang++ -arch arm64 -std=gnu++17 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I'/Users/buurensv/Dropbox/Package/mice/mice/revdep/library.noindex/mice/new/Rcpp/include' -I'/Users/buurensv/Dropbox/Package/mice/mice/revdep/library.noindex/clusterMI/RcppArmadillo/include' -I/opt/R/arm64/include -I/opt/homebrew/include    -fPIC  -falign-functions=64 -Wall -g -O2   -c RcppExports.cpp -o RcppExports.o
clang++ -arch arm64 -std=gnu++17 -dynamiclib -Wl,-headerpad_max_install_names -undefined dynamic_lookup -L/Library/Frameworks/R.framework/Resources/lib -L/opt/R/arm64/lib -L/opt/homebrew/lib -o clusterMI.so 1_Main.o 2_DataParam.o RcppExports.o -L/Library/Frameworks/R.framework/Resources/lib -lRlapack -L/Library/Frameworks/R.framework/Resources/lib -lRblas -L/opt/gfortran/lib/gcc/aarch64-apple-darwin20.0/14.2.0 -L/opt/gfortran/lib -lemutls_w -lheapt_w -lgfortran -lquadmath -F/Library/Frameworks/R.framework/.. -framework R
ld: warning: search path '/opt/gfortran/lib/gcc/aarch64-apple-darwin20.0/14.2.0' not found
ld: library 'emutls_w' not found
clang++: error: linker command failed with exit code 1 (use -v to see invocation)
make: *** [clusterMI.so] Error 1
ERROR: compilation failed for package ‘clusterMI’
* removing ‘/Users/buurensv/Dropbox/Package/mice/mice/revdep/checks.noindex/clusterMI/new/clusterMI.Rcheck/clusterMI’


```
### CRAN

```
* installing *source* package ‘clusterMI’ ...
** this is package ‘clusterMI’ version ‘1.5’
** package ‘clusterMI’ successfully unpacked and MD5 sums checked
** using staged installation
** libs
using C++ compiler: ‘Apple clang version 17.0.0 (clang-1700.0.13.3)’
using SDK: ‘’
clang++ -arch arm64 -std=gnu++17 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I'/Users/buurensv/Dropbox/Package/mice/mice/revdep/library.noindex/mice/old/Rcpp/include' -I'/Users/buurensv/Dropbox/Package/mice/mice/revdep/library.noindex/clusterMI/RcppArmadillo/include' -I/opt/R/arm64/include -I/opt/homebrew/include    -fPIC  -falign-functions=64 -Wall -g -O2   -c 1_Main.cpp -o 1_Main.o
clang++ -arch arm64 -std=gnu++17 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I'/Users/buurensv/Dropbox/Package/mice/mice/revdep/library.noindex/mice/old/Rcpp/include' -I'/Users/buurensv/Dropbox/Package/mice/mice/revdep/library.noindex/clusterMI/RcppArmadillo/include' -I/opt/R/arm64/include -I/opt/homebrew/include    -fPIC  -falign-functions=64 -Wall -g -O2   -c 2_DataParam.cpp -o 2_DataParam.o
clang++ -arch arm64 -std=gnu++17 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I'/Users/buurensv/Dropbox/Package/mice/mice/revdep/library.noindex/mice/old/Rcpp/include' -I'/Users/buurensv/Dropbox/Package/mice/mice/revdep/library.noindex/clusterMI/RcppArmadillo/include' -I/opt/R/arm64/include -I/opt/homebrew/include    -fPIC  -falign-functions=64 -Wall -g -O2   -c RcppExports.cpp -o RcppExports.o
clang++ -arch arm64 -std=gnu++17 -dynamiclib -Wl,-headerpad_max_install_names -undefined dynamic_lookup -L/Library/Frameworks/R.framework/Resources/lib -L/opt/R/arm64/lib -L/opt/homebrew/lib -o clusterMI.so 1_Main.o 2_DataParam.o RcppExports.o -L/Library/Frameworks/R.framework/Resources/lib -lRlapack -L/Library/Frameworks/R.framework/Resources/lib -lRblas -L/opt/gfortran/lib/gcc/aarch64-apple-darwin20.0/14.2.0 -L/opt/gfortran/lib -lemutls_w -lheapt_w -lgfortran -lquadmath -F/Library/Frameworks/R.framework/.. -framework R
ld: warning: search path '/opt/gfortran/lib/gcc/aarch64-apple-darwin20.0/14.2.0' not found
ld: library 'emutls_w' not found
clang++: error: linker command failed with exit code 1 (use -v to see invocation)
make: *** [clusterMI.so] Error 1
ERROR: compilation failed for package ‘clusterMI’
* removing ‘/Users/buurensv/Dropbox/Package/mice/mice/revdep/checks.noindex/clusterMI/old/clusterMI.Rcheck/clusterMI’


```
# dynr

<details>

* Version: 0.1.16-105
* GitHub: https://github.com/mhunter1/dynr
* Source code: https://github.com/cran/dynr
* Date/Publication: 2023-11-28 05:20:05 UTC
* Number of recursive dependencies: 147

Run `revdepcheck::revdep_details(, "dynr")` for more info

</details>

## In both

*   checking whether package ‘dynr’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/buurensv/Dropbox/Package/mice/mice/revdep/checks.noindex/dynr/new/dynr.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘dynr’ ...
** this is package ‘dynr’ version ‘0.1.16-105’
** package ‘dynr’ successfully unpacked and MD5 sums checked
** using staged installation
checking for gsl-config... no
configure: error: gsl-config not found, is GSL installed?
ERROR: configuration failed for package ‘dynr’
* removing ‘/Users/buurensv/Dropbox/Package/mice/mice/revdep/checks.noindex/dynr/new/dynr.Rcheck/dynr’


```
### CRAN

```
* installing *source* package ‘dynr’ ...
** this is package ‘dynr’ version ‘0.1.16-105’
** package ‘dynr’ successfully unpacked and MD5 sums checked
** using staged installation
checking for gsl-config... no
configure: error: gsl-config not found, is GSL installed?
ERROR: configuration failed for package ‘dynr’
* removing ‘/Users/buurensv/Dropbox/Package/mice/mice/revdep/checks.noindex/dynr/old/dynr.Rcheck/dynr’


```
# Hmisc

<details>

* Version: 5.2-3
* GitHub: NA
* Source code: https://github.com/cran/Hmisc
* Date/Publication: 2025-03-16 15:40:02 UTC
* Number of recursive dependencies: 174

Run `revdepcheck::revdep_details(, "Hmisc")` for more info

</details>

## In both

*   checking whether package ‘Hmisc’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/buurensv/Dropbox/Package/mice/mice/revdep/checks.noindex/Hmisc/new/Hmisc.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘Hmisc’ ...
** this is package ‘Hmisc’ version ‘5.2-3’
** package ‘Hmisc’ successfully unpacked and MD5 sums checked
** using staged installation
** libs
using C compiler: ‘Apple clang version 17.0.0 (clang-1700.0.13.3)’
using Fortran compiler: ‘GNU Fortran (GCC) 12.2.0’
using SDK: ‘’
clang -arch arm64 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG   -I/opt/R/arm64/include -I/opt/homebrew/include    -fPIC  -falign-functions=64 -Wall -g -O2  -c Hmisc.c -o Hmisc.o
/opt/gfortran/bin/gfortran -arch arm64  -fPIC  -Wall -g -O2  -c  cidxcn.f90 -o cidxcn.o
...
/opt/gfortran/bin/gfortran -arch arm64  -fPIC  -Wall -g -O2  -c  rcorr.f90 -o rcorr.o
clang -arch arm64 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG   -I/opt/R/arm64/include -I/opt/homebrew/include    -fPIC  -falign-functions=64 -Wall -g -O2  -c string_box.c -o string_box.o
/opt/gfortran/bin/gfortran -arch arm64  -fPIC  -Wall -g -O2  -c  wclosest.f90 -o wclosest.o
clang -arch arm64 -dynamiclib -Wl,-headerpad_max_install_names -undefined dynamic_lookup -L/Library/Frameworks/R.framework/Resources/lib -L/opt/R/arm64/lib -L/opt/homebrew/lib -o Hmisc.so Hmisc.o cidxcn.o cidxcp.o cutgn.o hlqest.o hoeffd.o init.o jacklins.o largrec.o mChoice.o maxempr.o nstr.o ranksort.o rcorr.o string_box.o wclosest.o -L/opt/gfortran/lib/gcc/aarch64-apple-darwin20.0/14.2.0 -L/opt/gfortran/lib -lemutls_w -lheapt_w -lgfortran -lquadmath -F/Library/Frameworks/R.framework/.. -framework R
ld: warning: search path '/opt/gfortran/lib/gcc/aarch64-apple-darwin20.0/14.2.0' not found
ld: library 'emutls_w' not found
clang: error: linker command failed with exit code 1 (use -v to see invocation)
make: *** [Hmisc.so] Error 1
ERROR: compilation failed for package ‘Hmisc’
* removing ‘/Users/buurensv/Dropbox/Package/mice/mice/revdep/checks.noindex/Hmisc/new/Hmisc.Rcheck/Hmisc’


```
### CRAN

```
* installing *source* package ‘Hmisc’ ...
** this is package ‘Hmisc’ version ‘5.2-3’
** package ‘Hmisc’ successfully unpacked and MD5 sums checked
** using staged installation
** libs
using C compiler: ‘Apple clang version 17.0.0 (clang-1700.0.13.3)’
using Fortran compiler: ‘GNU Fortran (GCC) 12.2.0’
using SDK: ‘’
clang -arch arm64 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG   -I/opt/R/arm64/include -I/opt/homebrew/include    -fPIC  -falign-functions=64 -Wall -g -O2  -c Hmisc.c -o Hmisc.o
/opt/gfortran/bin/gfortran -arch arm64  -fPIC  -Wall -g -O2  -c  cidxcn.f90 -o cidxcn.o
...
/opt/gfortran/bin/gfortran -arch arm64  -fPIC  -Wall -g -O2  -c  rcorr.f90 -o rcorr.o
clang -arch arm64 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG   -I/opt/R/arm64/include -I/opt/homebrew/include    -fPIC  -falign-functions=64 -Wall -g -O2  -c string_box.c -o string_box.o
/opt/gfortran/bin/gfortran -arch arm64  -fPIC  -Wall -g -O2  -c  wclosest.f90 -o wclosest.o
clang -arch arm64 -dynamiclib -Wl,-headerpad_max_install_names -undefined dynamic_lookup -L/Library/Frameworks/R.framework/Resources/lib -L/opt/R/arm64/lib -L/opt/homebrew/lib -o Hmisc.so Hmisc.o cidxcn.o cidxcp.o cutgn.o hlqest.o hoeffd.o init.o jacklins.o largrec.o mChoice.o maxempr.o nstr.o ranksort.o rcorr.o string_box.o wclosest.o -L/opt/gfortran/lib/gcc/aarch64-apple-darwin20.0/14.2.0 -L/opt/gfortran/lib -lemutls_w -lheapt_w -lgfortran -lquadmath -F/Library/Frameworks/R.framework/.. -framework R
ld: warning: search path '/opt/gfortran/lib/gcc/aarch64-apple-darwin20.0/14.2.0' not found
ld: library 'emutls_w' not found
clang: error: linker command failed with exit code 1 (use -v to see invocation)
make: *** [Hmisc.so] Error 1
ERROR: compilation failed for package ‘Hmisc’
* removing ‘/Users/buurensv/Dropbox/Package/mice/mice/revdep/checks.noindex/Hmisc/old/Hmisc.Rcheck/Hmisc’


```
# logistf

<details>

* Version: 1.26.1
* GitHub: https://github.com/georgheinze/logistf
* Source code: https://github.com/cran/logistf
* Date/Publication: 2025-04-16 15:50:02 UTC
* Number of recursive dependencies: 69

Run `revdepcheck::revdep_details(, "logistf")` for more info

</details>

## In both

*   checking whether package ‘logistf’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/buurensv/Dropbox/Package/mice/mice/revdep/checks.noindex/logistf/new/logistf.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘logistf’ ...
** this is package ‘logistf’ version ‘1.26.1’
** package ‘logistf’ successfully unpacked and MD5 sums checked
** using staged installation
** libs
using C compiler: ‘Apple clang version 17.0.0 (clang-1700.0.13.3)’
using SDK: ‘’
clang -arch arm64 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG   -I/opt/R/arm64/include -I/opt/homebrew/include    -fPIC  -falign-functions=64 -Wall -g -O2  -c init.c -o init.o
clang -arch arm64 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG   -I/opt/R/arm64/include -I/opt/homebrew/include    -fPIC  -falign-functions=64 -Wall -g -O2  -c logistf.c -o logistf.o
clang -arch arm64 -dynamiclib -Wl,-headerpad_max_install_names -undefined dynamic_lookup -L/Library/Frameworks/R.framework/Resources/lib -L/opt/R/arm64/lib -L/opt/homebrew/lib -o logistf.so init.o logistf.o -L/Library/Frameworks/R.framework/Resources/lib -lRlapack -L/Library/Frameworks/R.framework/Resources/lib -lRblas -L/opt/gfortran/lib/gcc/aarch64-apple-darwin20.0/14.2.0 -L/opt/gfortran/lib -lemutls_w -lheapt_w -lgfortran -lquadmath -F/Library/Frameworks/R.framework/.. -framework R
ld: warning: search path '/opt/gfortran/lib/gcc/aarch64-apple-darwin20.0/14.2.0' not found
ld: library 'emutls_w' not found
clang: error: linker command failed with exit code 1 (use -v to see invocation)
make: *** [logistf.so] Error 1
ERROR: compilation failed for package ‘logistf’
* removing ‘/Users/buurensv/Dropbox/Package/mice/mice/revdep/checks.noindex/logistf/new/logistf.Rcheck/logistf’


```
### CRAN

```
* installing *source* package ‘logistf’ ...
** this is package ‘logistf’ version ‘1.26.1’
** package ‘logistf’ successfully unpacked and MD5 sums checked
** using staged installation
** libs
using C compiler: ‘Apple clang version 17.0.0 (clang-1700.0.13.3)’
using SDK: ‘’
clang -arch arm64 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG   -I/opt/R/arm64/include -I/opt/homebrew/include    -fPIC  -falign-functions=64 -Wall -g -O2  -c init.c -o init.o
clang -arch arm64 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG   -I/opt/R/arm64/include -I/opt/homebrew/include    -fPIC  -falign-functions=64 -Wall -g -O2  -c logistf.c -o logistf.o
clang -arch arm64 -dynamiclib -Wl,-headerpad_max_install_names -undefined dynamic_lookup -L/Library/Frameworks/R.framework/Resources/lib -L/opt/R/arm64/lib -L/opt/homebrew/lib -o logistf.so init.o logistf.o -L/Library/Frameworks/R.framework/Resources/lib -lRlapack -L/Library/Frameworks/R.framework/Resources/lib -lRblas -L/opt/gfortran/lib/gcc/aarch64-apple-darwin20.0/14.2.0 -L/opt/gfortran/lib -lemutls_w -lheapt_w -lgfortran -lquadmath -F/Library/Frameworks/R.framework/.. -framework R
ld: warning: search path '/opt/gfortran/lib/gcc/aarch64-apple-darwin20.0/14.2.0' not found
ld: library 'emutls_w' not found
clang: error: linker command failed with exit code 1 (use -v to see invocation)
make: *** [logistf.so] Error 1
ERROR: compilation failed for package ‘logistf’
* removing ‘/Users/buurensv/Dropbox/Package/mice/mice/revdep/checks.noindex/logistf/old/logistf.Rcheck/logistf’


```
# miceadds

<details>

* Version: 3.17-44
* GitHub: https://github.com/alexanderrobitzsch/miceadds
* Source code: https://github.com/cran/miceadds
* Date/Publication: 2024-01-09 10:10:02 UTC
* Number of recursive dependencies: 156

Run `revdepcheck::revdep_details(, "miceadds")` for more info

</details>

## In both

*   checking whether package ‘miceadds’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/buurensv/Dropbox/Package/mice/mice/revdep/checks.noindex/miceadds/new/miceadds.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘miceadds’ ...
** this is package ‘miceadds’ version ‘3.17-44’
** package ‘miceadds’ successfully unpacked and MD5 sums checked
** using staged installation
** libs
using C++ compiler: ‘Apple clang version 17.0.0 (clang-1700.0.13.3)’
using SDK: ‘’
clang++ -arch arm64 -std=gnu++17 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -I../inst/include -DBOOST_NO_LONG_LONG -DBOOST_NO_AUTO_PTR -DRCPP_USE_UNWIND_PROTECT -I'/Users/buurensv/Dropbox/Package/mice/mice/revdep/library.noindex/mice/new/Rcpp/include' -I'/Users/buurensv/Dropbox/Package/mice/mice/revdep/library.noindex/miceadds/RcppArmadillo/include' -I/opt/R/arm64/include -I/opt/homebrew/include    -fPIC  -falign-functions=64 -Wall -g -O2   -c RcppExports.cpp -o RcppExports.o
clang++ -arch arm64 -std=gnu++17 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -I../inst/include -DBOOST_NO_LONG_LONG -DBOOST_NO_AUTO_PTR -DRCPP_USE_UNWIND_PROTECT -I'/Users/buurensv/Dropbox/Package/mice/mice/revdep/library.noindex/mice/new/Rcpp/include' -I'/Users/buurensv/Dropbox/Package/mice/mice/revdep/library.noindex/miceadds/RcppArmadillo/include' -I/opt/R/arm64/include -I/opt/homebrew/include    -fPIC  -falign-functions=64 -Wall -g -O2   -c miceadds_rcpp_create_interactions.cpp -o miceadds_rcpp_create_interactions.o
clang++ -arch arm64 -std=gnu++17 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -I../inst/include -DBOOST_NO_LONG_LONG -DBOOST_NO_AUTO_PTR -DRCPP_USE_UNWIND_PROTECT -I'/Users/buurensv/Dropbox/Package/mice/mice/revdep/library.noindex/mice/new/Rcpp/include' -I'/Users/buurensv/Dropbox/Package/mice/mice/revdep/library.noindex/miceadds/RcppArmadillo/include' -I/opt/R/arm64/include -I/opt/homebrew/include    -fPIC  -falign-functions=64 -Wall -g -O2   -c miceadds_rcpp_kernelpls_1dim.cpp -o miceadds_rcpp_kernelpls_1dim.o
...
clang++ -arch arm64 -std=gnu++17 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -I../inst/include -DBOOST_NO_LONG_LONG -DBOOST_NO_AUTO_PTR -DRCPP_USE_UNWIND_PROTECT -I'/Users/buurensv/Dropbox/Package/mice/mice/revdep/library.noindex/mice/new/Rcpp/include' -I'/Users/buurensv/Dropbox/Package/mice/mice/revdep/library.noindex/miceadds/RcppArmadillo/include' -I/opt/R/arm64/include -I/opt/homebrew/include    -fPIC  -falign-functions=64 -Wall -g -O2   -c miceadds_rcpp_scale.cpp -o miceadds_rcpp_scale.o
clang++ -arch arm64 -std=gnu++17 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -I../inst/include -DBOOST_NO_LONG_LONG -DBOOST_NO_AUTO_PTR -DRCPP_USE_UNWIND_PROTECT -I'/Users/buurensv/Dropbox/Package/mice/mice/revdep/library.noindex/mice/new/Rcpp/include' -I'/Users/buurensv/Dropbox/Package/mice/mice/revdep/library.noindex/miceadds/RcppArmadillo/include' -I/opt/R/arm64/include -I/opt/homebrew/include    -fPIC  -falign-functions=64 -Wall -g -O2   -c miceadds_rcpp_weighted_cor.cpp -o miceadds_rcpp_weighted_cor.o
clang++ -arch arm64 -std=gnu++17 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -I../inst/include -DBOOST_NO_LONG_LONG -DBOOST_NO_AUTO_PTR -DRCPP_USE_UNWIND_PROTECT -I'/Users/buurensv/Dropbox/Package/mice/mice/revdep/library.noindex/mice/new/Rcpp/include' -I'/Users/buurensv/Dropbox/Package/mice/mice/revdep/library.noindex/miceadds/RcppArmadillo/include' -I/opt/R/arm64/include -I/opt/homebrew/include    -fPIC  -falign-functions=64 -Wall -g -O2   -c miceadds_rcpp_weighted_pmm.cpp -o miceadds_rcpp_weighted_pmm.o
clang++ -arch arm64 -std=gnu++17 -dynamiclib -Wl,-headerpad_max_install_names -undefined dynamic_lookup -L/Library/Frameworks/R.framework/Resources/lib -L/opt/R/arm64/lib -L/opt/homebrew/lib -o miceadds.so RcppExports.o miceadds_rcpp_create_interactions.o miceadds_rcpp_kernelpls_1dim.o miceadds_rcpp_ml_mcmc_sampler.o miceadds_rcpp_ml_mcmc_sub.o miceadds_rcpp_pmm6.o miceadds_rcpp_scale.o miceadds_rcpp_weighted_cor.o miceadds_rcpp_weighted_pmm.o -L/Library/Frameworks/R.framework/Resources/lib -lRlapack -L/Library/Frameworks/R.framework/Resources/lib -lRblas -L/opt/gfortran/lib/gcc/aarch64-apple-darwin20.0/14.2.0 -L/opt/gfortran/lib -lemutls_w -lheapt_w -lgfortran -lquadmath -F/Library/Frameworks/R.framework/.. -framework R
ld: warning: search path '/opt/gfortran/lib/gcc/aarch64-apple-darwin20.0/14.2.0' not found
ld: library 'emutls_w' not found
clang++: error: linker command failed with exit code 1 (use -v to see invocation)
make: *** [miceadds.so] Error 1
ERROR: compilation failed for package ‘miceadds’
* removing ‘/Users/buurensv/Dropbox/Package/mice/mice/revdep/checks.noindex/miceadds/new/miceadds.Rcheck/miceadds’


```
### CRAN

```
* installing *source* package ‘miceadds’ ...
** this is package ‘miceadds’ version ‘3.17-44’
** package ‘miceadds’ successfully unpacked and MD5 sums checked
** using staged installation
** libs
using C++ compiler: ‘Apple clang version 17.0.0 (clang-1700.0.13.3)’
using SDK: ‘’
clang++ -arch arm64 -std=gnu++17 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -I../inst/include -DBOOST_NO_LONG_LONG -DBOOST_NO_AUTO_PTR -DRCPP_USE_UNWIND_PROTECT -I'/Users/buurensv/Dropbox/Package/mice/mice/revdep/library.noindex/mice/old/Rcpp/include' -I'/Users/buurensv/Dropbox/Package/mice/mice/revdep/library.noindex/miceadds/RcppArmadillo/include' -I/opt/R/arm64/include -I/opt/homebrew/include    -fPIC  -falign-functions=64 -Wall -g -O2   -c RcppExports.cpp -o RcppExports.o
clang++ -arch arm64 -std=gnu++17 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -I../inst/include -DBOOST_NO_LONG_LONG -DBOOST_NO_AUTO_PTR -DRCPP_USE_UNWIND_PROTECT -I'/Users/buurensv/Dropbox/Package/mice/mice/revdep/library.noindex/mice/old/Rcpp/include' -I'/Users/buurensv/Dropbox/Package/mice/mice/revdep/library.noindex/miceadds/RcppArmadillo/include' -I/opt/R/arm64/include -I/opt/homebrew/include    -fPIC  -falign-functions=64 -Wall -g -O2   -c miceadds_rcpp_create_interactions.cpp -o miceadds_rcpp_create_interactions.o
clang++ -arch arm64 -std=gnu++17 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -I../inst/include -DBOOST_NO_LONG_LONG -DBOOST_NO_AUTO_PTR -DRCPP_USE_UNWIND_PROTECT -I'/Users/buurensv/Dropbox/Package/mice/mice/revdep/library.noindex/mice/old/Rcpp/include' -I'/Users/buurensv/Dropbox/Package/mice/mice/revdep/library.noindex/miceadds/RcppArmadillo/include' -I/opt/R/arm64/include -I/opt/homebrew/include    -fPIC  -falign-functions=64 -Wall -g -O2   -c miceadds_rcpp_kernelpls_1dim.cpp -o miceadds_rcpp_kernelpls_1dim.o
...
clang++ -arch arm64 -std=gnu++17 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -I../inst/include -DBOOST_NO_LONG_LONG -DBOOST_NO_AUTO_PTR -DRCPP_USE_UNWIND_PROTECT -I'/Users/buurensv/Dropbox/Package/mice/mice/revdep/library.noindex/mice/old/Rcpp/include' -I'/Users/buurensv/Dropbox/Package/mice/mice/revdep/library.noindex/miceadds/RcppArmadillo/include' -I/opt/R/arm64/include -I/opt/homebrew/include    -fPIC  -falign-functions=64 -Wall -g -O2   -c miceadds_rcpp_scale.cpp -o miceadds_rcpp_scale.o
clang++ -arch arm64 -std=gnu++17 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -I../inst/include -DBOOST_NO_LONG_LONG -DBOOST_NO_AUTO_PTR -DRCPP_USE_UNWIND_PROTECT -I'/Users/buurensv/Dropbox/Package/mice/mice/revdep/library.noindex/mice/old/Rcpp/include' -I'/Users/buurensv/Dropbox/Package/mice/mice/revdep/library.noindex/miceadds/RcppArmadillo/include' -I/opt/R/arm64/include -I/opt/homebrew/include    -fPIC  -falign-functions=64 -Wall -g -O2   -c miceadds_rcpp_weighted_cor.cpp -o miceadds_rcpp_weighted_cor.o
clang++ -arch arm64 -std=gnu++17 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -I../inst/include -DBOOST_NO_LONG_LONG -DBOOST_NO_AUTO_PTR -DRCPP_USE_UNWIND_PROTECT -I'/Users/buurensv/Dropbox/Package/mice/mice/revdep/library.noindex/mice/old/Rcpp/include' -I'/Users/buurensv/Dropbox/Package/mice/mice/revdep/library.noindex/miceadds/RcppArmadillo/include' -I/opt/R/arm64/include -I/opt/homebrew/include    -fPIC  -falign-functions=64 -Wall -g -O2   -c miceadds_rcpp_weighted_pmm.cpp -o miceadds_rcpp_weighted_pmm.o
clang++ -arch arm64 -std=gnu++17 -dynamiclib -Wl,-headerpad_max_install_names -undefined dynamic_lookup -L/Library/Frameworks/R.framework/Resources/lib -L/opt/R/arm64/lib -L/opt/homebrew/lib -o miceadds.so RcppExports.o miceadds_rcpp_create_interactions.o miceadds_rcpp_kernelpls_1dim.o miceadds_rcpp_ml_mcmc_sampler.o miceadds_rcpp_ml_mcmc_sub.o miceadds_rcpp_pmm6.o miceadds_rcpp_scale.o miceadds_rcpp_weighted_cor.o miceadds_rcpp_weighted_pmm.o -L/Library/Frameworks/R.framework/Resources/lib -lRlapack -L/Library/Frameworks/R.framework/Resources/lib -lRblas -L/opt/gfortran/lib/gcc/aarch64-apple-darwin20.0/14.2.0 -L/opt/gfortran/lib -lemutls_w -lheapt_w -lgfortran -lquadmath -F/Library/Frameworks/R.framework/.. -framework R
ld: warning: search path '/opt/gfortran/lib/gcc/aarch64-apple-darwin20.0/14.2.0' not found
ld: library 'emutls_w' not found
clang++: error: linker command failed with exit code 1 (use -v to see invocation)
make: *** [miceadds.so] Error 1
ERROR: compilation failed for package ‘miceadds’
* removing ‘/Users/buurensv/Dropbox/Package/mice/mice/revdep/checks.noindex/miceadds/old/miceadds.Rcheck/miceadds’


```
# miceFast

<details>

* Version: 0.8.5
* GitHub: https://github.com/Polkas/miceFast
* Source code: https://github.com/cran/miceFast
* Date/Publication: 2025-02-03 22:20:02 UTC
* Number of recursive dependencies: 112

Run `revdepcheck::revdep_details(, "miceFast")` for more info

</details>

## In both

*   checking whether package ‘miceFast’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/buurensv/Dropbox/Package/mice/mice/revdep/checks.noindex/miceFast/new/miceFast.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘miceFast’ ...
** this is package ‘miceFast’ version ‘0.8.5’
** package ‘miceFast’ successfully unpacked and MD5 sums checked
** using staged installation
** libs
using C++ compiler: ‘Apple clang version 17.0.0 (clang-1700.0.13.3)’
using SDK: ‘’
clang++ -arch arm64 -std=gnu++17 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I'/Users/buurensv/Dropbox/Package/mice/mice/revdep/library.noindex/mice/new/Rcpp/include' -I'/Users/buurensv/Dropbox/Package/mice/mice/revdep/library.noindex/miceFast/RcppArmadillo/include' -I/opt/R/arm64/include -I/opt/homebrew/include    -fPIC  -falign-functions=64 -Wall -g -O2   -c R_funs.cpp -o R_funs.o
clang++ -arch arm64 -std=gnu++17 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I'/Users/buurensv/Dropbox/Package/mice/mice/revdep/library.noindex/mice/new/Rcpp/include' -I'/Users/buurensv/Dropbox/Package/mice/mice/revdep/library.noindex/miceFast/RcppArmadillo/include' -I/opt/R/arm64/include -I/opt/homebrew/include    -fPIC  -falign-functions=64 -Wall -g -O2   -c RcppExports.cpp -o RcppExports.o
clang++ -arch arm64 -std=gnu++17 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I'/Users/buurensv/Dropbox/Package/mice/mice/revdep/library.noindex/mice/new/Rcpp/include' -I'/Users/buurensv/Dropbox/Package/mice/mice/revdep/library.noindex/miceFast/RcppArmadillo/include' -I/opt/R/arm64/include -I/opt/homebrew/include    -fPIC  -falign-functions=64 -Wall -g -O2   -c corrData.cpp -o corrData.o
...
  472 | static R_CallMethodDef callMethods[] = {
      |                        ^~~~~~~~~~~
1 warning generated.
clang++ -arch arm64 -std=gnu++17 -dynamiclib -Wl,-headerpad_max_install_names -undefined dynamic_lookup -L/Library/Frameworks/R.framework/Resources/lib -L/opt/R/arm64/lib -L/opt/homebrew/lib -o miceFast.so R_funs.o RcppExports.o corrData.o miceFast_additfunc.o miceFast_class.o miceFast_quantmodels.o -L/Library/Frameworks/R.framework/Resources/lib -lRlapack -L/Library/Frameworks/R.framework/Resources/lib -lRblas -L/opt/gfortran/lib/gcc/aarch64-apple-darwin20.0/14.2.0 -L/opt/gfortran/lib -lemutls_w -lheapt_w -lgfortran -lquadmath -F/Library/Frameworks/R.framework/.. -framework R
ld: warning: search path '/opt/gfortran/lib/gcc/aarch64-apple-darwin20.0/14.2.0' not found
ld: library 'emutls_w' not found
clang++: error: linker command failed with exit code 1 (use -v to see invocation)
make: *** [miceFast.so] Error 1
ERROR: compilation failed for package ‘miceFast’
* removing ‘/Users/buurensv/Dropbox/Package/mice/mice/revdep/checks.noindex/miceFast/new/miceFast.Rcheck/miceFast’


```
### CRAN

```
* installing *source* package ‘miceFast’ ...
** this is package ‘miceFast’ version ‘0.8.5’
** package ‘miceFast’ successfully unpacked and MD5 sums checked
** using staged installation
** libs
using C++ compiler: ‘Apple clang version 17.0.0 (clang-1700.0.13.3)’
using SDK: ‘’
clang++ -arch arm64 -std=gnu++17 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I'/Users/buurensv/Dropbox/Package/mice/mice/revdep/library.noindex/mice/old/Rcpp/include' -I'/Users/buurensv/Dropbox/Package/mice/mice/revdep/library.noindex/miceFast/RcppArmadillo/include' -I/opt/R/arm64/include -I/opt/homebrew/include    -fPIC  -falign-functions=64 -Wall -g -O2   -c R_funs.cpp -o R_funs.o
clang++ -arch arm64 -std=gnu++17 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I'/Users/buurensv/Dropbox/Package/mice/mice/revdep/library.noindex/mice/old/Rcpp/include' -I'/Users/buurensv/Dropbox/Package/mice/mice/revdep/library.noindex/miceFast/RcppArmadillo/include' -I/opt/R/arm64/include -I/opt/homebrew/include    -fPIC  -falign-functions=64 -Wall -g -O2   -c RcppExports.cpp -o RcppExports.o
clang++ -arch arm64 -std=gnu++17 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I'/Users/buurensv/Dropbox/Package/mice/mice/revdep/library.noindex/mice/old/Rcpp/include' -I'/Users/buurensv/Dropbox/Package/mice/mice/revdep/library.noindex/miceFast/RcppArmadillo/include' -I/opt/R/arm64/include -I/opt/homebrew/include    -fPIC  -falign-functions=64 -Wall -g -O2   -c corrData.cpp -o corrData.o
...
  472 | static R_CallMethodDef callMethods[] = {
      |                        ^~~~~~~~~~~
1 warning generated.
clang++ -arch arm64 -std=gnu++17 -dynamiclib -Wl,-headerpad_max_install_names -undefined dynamic_lookup -L/Library/Frameworks/R.framework/Resources/lib -L/opt/R/arm64/lib -L/opt/homebrew/lib -o miceFast.so R_funs.o RcppExports.o corrData.o miceFast_additfunc.o miceFast_class.o miceFast_quantmodels.o -L/Library/Frameworks/R.framework/Resources/lib -lRlapack -L/Library/Frameworks/R.framework/Resources/lib -lRblas -L/opt/gfortran/lib/gcc/aarch64-apple-darwin20.0/14.2.0 -L/opt/gfortran/lib -lemutls_w -lheapt_w -lgfortran -lquadmath -F/Library/Frameworks/R.framework/.. -framework R
ld: warning: search path '/opt/gfortran/lib/gcc/aarch64-apple-darwin20.0/14.2.0' not found
ld: library 'emutls_w' not found
clang++: error: linker command failed with exit code 1 (use -v to see invocation)
make: *** [miceFast.so] Error 1
ERROR: compilation failed for package ‘miceFast’
* removing ‘/Users/buurensv/Dropbox/Package/mice/mice/revdep/checks.noindex/miceFast/old/miceFast.Rcheck/miceFast’


```
# mixgb

<details>

* Version: 1.5.3
* GitHub: https://github.com/agnesdeng/mixgb
* Source code: https://github.com/cran/mixgb
* Date/Publication: 2025-04-06 23:30:02 UTC
* Number of recursive dependencies: 102

Run `revdepcheck::revdep_details(, "mixgb")` for more info

</details>

## In both

*   checking whether package ‘mixgb’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/buurensv/Dropbox/Package/mice/mice/revdep/checks.noindex/mixgb/new/mixgb.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘mixgb’ ...
** this is package ‘mixgb’ version ‘1.5.3’
** package ‘mixgb’ successfully unpacked and MD5 sums checked
** using staged installation
** libs
using C++ compiler: ‘Apple clang version 17.0.0 (clang-1700.0.13.3)’
using SDK: ‘’
clang++ -arch arm64 -std=gnu++17 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I'/Users/buurensv/Dropbox/Package/mice/mice/revdep/library.noindex/mice/new/Rcpp/include' -I'/Users/buurensv/Dropbox/Package/mice/mice/revdep/library.noindex/mixgb/RcppArmadillo/include' -I/opt/R/arm64/include -I/opt/homebrew/include    -fPIC  -falign-functions=64 -Wall -g -O2   -c RcppExports.cpp -o RcppExports.o
clang++ -arch arm64 -std=gnu++17 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I'/Users/buurensv/Dropbox/Package/mice/mice/revdep/library.noindex/mice/new/Rcpp/include' -I'/Users/buurensv/Dropbox/Package/mice/mice/revdep/library.noindex/mixgb/RcppArmadillo/include' -I/opt/R/arm64/include -I/opt/homebrew/include    -fPIC  -falign-functions=64 -Wall -g -O2   -c cbind_combo.cpp -o cbind_combo.o
clang++ -arch arm64 -std=gnu++17 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I'/Users/buurensv/Dropbox/Package/mice/mice/revdep/library.noindex/mice/new/Rcpp/include' -I'/Users/buurensv/Dropbox/Package/mice/mice/revdep/library.noindex/mixgb/RcppArmadillo/include' -I/opt/R/arm64/include -I/opt/homebrew/include    -fPIC  -falign-functions=64 -Wall -g -O2   -c cbind_combo0.cpp -o cbind_combo0.o
clang++ -arch arm64 -std=gnu++17 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I'/Users/buurensv/Dropbox/Package/mice/mice/revdep/library.noindex/mice/new/Rcpp/include' -I'/Users/buurensv/Dropbox/Package/mice/mice/revdep/library.noindex/mixgb/RcppArmadillo/include' -I/opt/R/arm64/include -I/opt/homebrew/include    -fPIC  -falign-functions=64 -Wall -g -O2   -c cbind_sparse_matrix.cpp -o cbind_sparse_matrix.o
clang++ -arch arm64 -std=gnu++17 -dynamiclib -Wl,-headerpad_max_install_names -undefined dynamic_lookup -L/Library/Frameworks/R.framework/Resources/lib -L/opt/R/arm64/lib -L/opt/homebrew/lib -o mixgb.so RcppExports.o cbind_combo.o cbind_combo0.o cbind_sparse_matrix.o -L/Library/Frameworks/R.framework/Resources/lib -lRlapack -L/Library/Frameworks/R.framework/Resources/lib -lRblas -L/opt/gfortran/lib/gcc/aarch64-apple-darwin20.0/14.2.0 -L/opt/gfortran/lib -lemutls_w -lheapt_w -lgfortran -lquadmath -F/Library/Frameworks/R.framework/.. -framework R
ld: warning: search path '/opt/gfortran/lib/gcc/aarch64-apple-darwin20.0/14.2.0' not found
ld: library 'emutls_w' not found
clang++: error: linker command failed with exit code 1 (use -v to see invocation)
make: *** [mixgb.so] Error 1
ERROR: compilation failed for package ‘mixgb’
* removing ‘/Users/buurensv/Dropbox/Package/mice/mice/revdep/checks.noindex/mixgb/new/mixgb.Rcheck/mixgb’


```
### CRAN

```
* installing *source* package ‘mixgb’ ...
** this is package ‘mixgb’ version ‘1.5.3’
** package ‘mixgb’ successfully unpacked and MD5 sums checked
** using staged installation
** libs
using C++ compiler: ‘Apple clang version 17.0.0 (clang-1700.0.13.3)’
using SDK: ‘’
clang++ -arch arm64 -std=gnu++17 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I'/Users/buurensv/Dropbox/Package/mice/mice/revdep/library.noindex/mice/old/Rcpp/include' -I'/Users/buurensv/Dropbox/Package/mice/mice/revdep/library.noindex/mixgb/RcppArmadillo/include' -I/opt/R/arm64/include -I/opt/homebrew/include    -fPIC  -falign-functions=64 -Wall -g -O2   -c RcppExports.cpp -o RcppExports.o
clang++ -arch arm64 -std=gnu++17 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I'/Users/buurensv/Dropbox/Package/mice/mice/revdep/library.noindex/mice/old/Rcpp/include' -I'/Users/buurensv/Dropbox/Package/mice/mice/revdep/library.noindex/mixgb/RcppArmadillo/include' -I/opt/R/arm64/include -I/opt/homebrew/include    -fPIC  -falign-functions=64 -Wall -g -O2   -c cbind_combo.cpp -o cbind_combo.o
clang++ -arch arm64 -std=gnu++17 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I'/Users/buurensv/Dropbox/Package/mice/mice/revdep/library.noindex/mice/old/Rcpp/include' -I'/Users/buurensv/Dropbox/Package/mice/mice/revdep/library.noindex/mixgb/RcppArmadillo/include' -I/opt/R/arm64/include -I/opt/homebrew/include    -fPIC  -falign-functions=64 -Wall -g -O2   -c cbind_combo0.cpp -o cbind_combo0.o
clang++ -arch arm64 -std=gnu++17 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I'/Users/buurensv/Dropbox/Package/mice/mice/revdep/library.noindex/mice/old/Rcpp/include' -I'/Users/buurensv/Dropbox/Package/mice/mice/revdep/library.noindex/mixgb/RcppArmadillo/include' -I/opt/R/arm64/include -I/opt/homebrew/include    -fPIC  -falign-functions=64 -Wall -g -O2   -c cbind_sparse_matrix.cpp -o cbind_sparse_matrix.o
clang++ -arch arm64 -std=gnu++17 -dynamiclib -Wl,-headerpad_max_install_names -undefined dynamic_lookup -L/Library/Frameworks/R.framework/Resources/lib -L/opt/R/arm64/lib -L/opt/homebrew/lib -o mixgb.so RcppExports.o cbind_combo.o cbind_combo0.o cbind_sparse_matrix.o -L/Library/Frameworks/R.framework/Resources/lib -lRlapack -L/Library/Frameworks/R.framework/Resources/lib -lRblas -L/opt/gfortran/lib/gcc/aarch64-apple-darwin20.0/14.2.0 -L/opt/gfortran/lib -lemutls_w -lheapt_w -lgfortran -lquadmath -F/Library/Frameworks/R.framework/.. -framework R
ld: warning: search path '/opt/gfortran/lib/gcc/aarch64-apple-darwin20.0/14.2.0' not found
ld: library 'emutls_w' not found
clang++: error: linker command failed with exit code 1 (use -v to see invocation)
make: *** [mixgb.so] Error 1
ERROR: compilation failed for package ‘mixgb’
* removing ‘/Users/buurensv/Dropbox/Package/mice/mice/revdep/checks.noindex/mixgb/old/mixgb.Rcheck/mixgb’


```
# RefBasedMI

<details>

* Version: 0.2.0
* GitHub: https://github.com/UCL/RefBasedMI
* Source code: https://github.com/cran/RefBasedMI
* Date/Publication: 2024-09-09 09:10:01 UTC
* Number of recursive dependencies: 107

Run `revdepcheck::revdep_details(, "RefBasedMI")` for more info

</details>

## In both

*   checking whether package ‘RefBasedMI’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/buurensv/Dropbox/Package/mice/mice/revdep/checks.noindex/RefBasedMI/new/RefBasedMI.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘RefBasedMI’ ...
** this is package ‘RefBasedMI’ version ‘0.2.0’
** package ‘RefBasedMI’ successfully unpacked and MD5 sums checked
** using staged installation
** libs
using C compiler: ‘Apple clang version 17.0.0 (clang-1700.0.13.3)’
using Fortran compiler: ‘GNU Fortran (GCC) 12.2.0’
using SDK: ‘’
/opt/gfortran/bin/gfortran -arch arm64  -fPIC  -Wall -g -O2  -c  program_constants.f90 -o program_constants.o
/opt/gfortran/bin/gfortran -arch arm64  -fPIC  -Wall -g -O2  -c  error_handler.f90 -o error_handler.o
...
/opt/gfortran/bin/gfortran -arch arm64  -fPIC  -Wall -g -O2  -c  norm_engine.f90 -o norm_engine.o
/opt/gfortran/bin/gfortran -arch arm64  -fPIC  -Wall -g -O2  -c  norm2.f90 -o norm2.o
clang -arch arm64 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG   -I/opt/R/arm64/include -I/opt/homebrew/include    -fPIC  -falign-functions=64 -Wall -g -O2  -c registerDynamicSymbol.c -o registerDynamicSymbol.o
clang -arch arm64 -dynamiclib -Wl,-headerpad_max_install_names -undefined dynamic_lookup -L/Library/Frameworks/R.framework/Resources/lib -L/opt/R/arm64/lib -L/opt/homebrew/lib -o RefBasedMI.so dynalloc.o error_handler.o matrix_methods.o norm2.o norm_engine.o program_constants.o quick_sort.o random_generator.o registerDynamicSymbol.o -L/opt/gfortran/lib/gcc/aarch64-apple-darwin20.0/14.2.0 -L/opt/gfortran/lib -lemutls_w -lheapt_w -lgfortran -lquadmath -F/Library/Frameworks/R.framework/.. -framework R
ld: warning: search path '/opt/gfortran/lib/gcc/aarch64-apple-darwin20.0/14.2.0' not found
ld: library 'emutls_w' not found
clang: error: linker command failed with exit code 1 (use -v to see invocation)
make: *** [RefBasedMI.so] Error 1
ERROR: compilation failed for package ‘RefBasedMI’
* removing ‘/Users/buurensv/Dropbox/Package/mice/mice/revdep/checks.noindex/RefBasedMI/new/RefBasedMI.Rcheck/RefBasedMI’


```
### CRAN

```
* installing *source* package ‘RefBasedMI’ ...
** this is package ‘RefBasedMI’ version ‘0.2.0’
** package ‘RefBasedMI’ successfully unpacked and MD5 sums checked
** using staged installation
** libs
using C compiler: ‘Apple clang version 17.0.0 (clang-1700.0.13.3)’
using Fortran compiler: ‘GNU Fortran (GCC) 12.2.0’
using SDK: ‘’
/opt/gfortran/bin/gfortran -arch arm64  -fPIC  -Wall -g -O2  -c  program_constants.f90 -o program_constants.o
/opt/gfortran/bin/gfortran -arch arm64  -fPIC  -Wall -g -O2  -c  error_handler.f90 -o error_handler.o
...
/opt/gfortran/bin/gfortran -arch arm64  -fPIC  -Wall -g -O2  -c  norm_engine.f90 -o norm_engine.o
/opt/gfortran/bin/gfortran -arch arm64  -fPIC  -Wall -g -O2  -c  norm2.f90 -o norm2.o
clang -arch arm64 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG   -I/opt/R/arm64/include -I/opt/homebrew/include    -fPIC  -falign-functions=64 -Wall -g -O2  -c registerDynamicSymbol.c -o registerDynamicSymbol.o
clang -arch arm64 -dynamiclib -Wl,-headerpad_max_install_names -undefined dynamic_lookup -L/Library/Frameworks/R.framework/Resources/lib -L/opt/R/arm64/lib -L/opt/homebrew/lib -o RefBasedMI.so dynalloc.o error_handler.o matrix_methods.o norm2.o norm_engine.o program_constants.o quick_sort.o random_generator.o registerDynamicSymbol.o -L/opt/gfortran/lib/gcc/aarch64-apple-darwin20.0/14.2.0 -L/opt/gfortran/lib -lemutls_w -lheapt_w -lgfortran -lquadmath -F/Library/Frameworks/R.framework/.. -framework R
ld: warning: search path '/opt/gfortran/lib/gcc/aarch64-apple-darwin20.0/14.2.0' not found
ld: library 'emutls_w' not found
clang: error: linker command failed with exit code 1 (use -v to see invocation)
make: *** [RefBasedMI.so] Error 1
ERROR: compilation failed for package ‘RefBasedMI’
* removing ‘/Users/buurensv/Dropbox/Package/mice/mice/revdep/checks.noindex/RefBasedMI/old/RefBasedMI.Rcheck/RefBasedMI’


```
# rms

<details>

* Version: 8.0-0
* GitHub: https://github.com/harrelfe/rms
* Source code: https://github.com/cran/rms
* Date/Publication: 2025-04-04 15:50:02 UTC
* Number of recursive dependencies: 151

Run `revdepcheck::revdep_details(, "rms")` for more info

</details>

## In both

*   checking whether package ‘rms’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/buurensv/Dropbox/Package/mice/mice/revdep/checks.noindex/rms/new/rms.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘rms’ ...
** this is package ‘rms’ version ‘8.0-0’
** package ‘rms’ successfully unpacked and MD5 sums checked
** using staged installation
** libs
using C compiler: ‘Apple clang version 17.0.0 (clang-1700.0.13.3)’
using Fortran compiler: ‘GNU Fortran (GCC) 12.2.0’
using SDK: ‘’
clang -arch arm64 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG   -I/opt/R/arm64/include -I/opt/homebrew/include    -fPIC  -falign-functions=64 -Wall -g -O2  -c init.c -o init.o
/opt/gfortran/bin/gfortran -arch arm64  -fPIC  -Wall -g -O2  -c  lrmll.f90 -o lrmll.o
...
      |                                                                   ^
note: ‘dpdf1.dim[0].ubound’ was declared here
/opt/gfortran/bin/gfortran -arch arm64  -fPIC  -Wall -g -O2  -c  robcovf.f90 -o robcovf.o
clang -arch arm64 -dynamiclib -Wl,-headerpad_max_install_names -undefined dynamic_lookup -L/Library/Frameworks/R.framework/Resources/lib -L/opt/R/arm64/lib -L/opt/homebrew/lib -o rms.so init.o lrmll.o mlmats.o ormll.o robcovf.o -L/opt/gfortran/lib/gcc/aarch64-apple-darwin20.0/14.2.0 -L/opt/gfortran/lib -lemutls_w -lheapt_w -lgfortran -lquadmath -F/Library/Frameworks/R.framework/.. -framework R
ld: warning: search path '/opt/gfortran/lib/gcc/aarch64-apple-darwin20.0/14.2.0' not found
ld: library 'emutls_w' not found
clang: error: linker command failed with exit code 1 (use -v to see invocation)
make: *** [rms.so] Error 1
ERROR: compilation failed for package ‘rms’
* removing ‘/Users/buurensv/Dropbox/Package/mice/mice/revdep/checks.noindex/rms/new/rms.Rcheck/rms’


```
### CRAN

```
* installing *source* package ‘rms’ ...
** this is package ‘rms’ version ‘8.0-0’
** package ‘rms’ successfully unpacked and MD5 sums checked
** using staged installation
** libs
using C compiler: ‘Apple clang version 17.0.0 (clang-1700.0.13.3)’
using Fortran compiler: ‘GNU Fortran (GCC) 12.2.0’
using SDK: ‘’
clang -arch arm64 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG   -I/opt/R/arm64/include -I/opt/homebrew/include    -fPIC  -falign-functions=64 -Wall -g -O2  -c init.c -o init.o
/opt/gfortran/bin/gfortran -arch arm64  -fPIC  -Wall -g -O2  -c  lrmll.f90 -o lrmll.o
...
      |                                                                   ^
note: ‘dpdf1.dim[0].ubound’ was declared here
/opt/gfortran/bin/gfortran -arch arm64  -fPIC  -Wall -g -O2  -c  robcovf.f90 -o robcovf.o
clang -arch arm64 -dynamiclib -Wl,-headerpad_max_install_names -undefined dynamic_lookup -L/Library/Frameworks/R.framework/Resources/lib -L/opt/R/arm64/lib -L/opt/homebrew/lib -o rms.so init.o lrmll.o mlmats.o ormll.o robcovf.o -L/opt/gfortran/lib/gcc/aarch64-apple-darwin20.0/14.2.0 -L/opt/gfortran/lib -lemutls_w -lheapt_w -lgfortran -lquadmath -F/Library/Frameworks/R.framework/.. -framework R
ld: warning: search path '/opt/gfortran/lib/gcc/aarch64-apple-darwin20.0/14.2.0' not found
ld: library 'emutls_w' not found
clang: error: linker command failed with exit code 1 (use -v to see invocation)
make: *** [rms.so] Error 1
ERROR: compilation failed for package ‘rms’
* removing ‘/Users/buurensv/Dropbox/Package/mice/mice/revdep/checks.noindex/rms/old/rms.Rcheck/rms’


```
# rmsb

<details>

* Version: 1.1-2
* GitHub: NA
* Source code: https://github.com/cran/rmsb
* Date/Publication: 2025-04-13 16:30:02 UTC
* Number of recursive dependencies: 137

Run `revdepcheck::revdep_details(, "rmsb")` for more info

</details>

## In both

*   checking whether package ‘rmsb’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/buurensv/Dropbox/Package/mice/mice/revdep/checks.noindex/rmsb/new/rmsb.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘rmsb’ ...
** this is package ‘rmsb’ version ‘1.1-2’
** package ‘rmsb’ successfully unpacked and MD5 sums checked
** using staged installation
Error in loadNamespace(x) : there is no package called ‘rstantools’
Calls: loadNamespace -> withRestarts -> withOneRestart -> doWithOneRestart
Execution halted
ERROR: configuration failed for package ‘rmsb’
* removing ‘/Users/buurensv/Dropbox/Package/mice/mice/revdep/checks.noindex/rmsb/new/rmsb.Rcheck/rmsb’


```
### CRAN

```
* installing *source* package ‘rmsb’ ...
** this is package ‘rmsb’ version ‘1.1-2’
** package ‘rmsb’ successfully unpacked and MD5 sums checked
** using staged installation
Error in loadNamespace(x) : there is no package called ‘rstantools’
Calls: loadNamespace -> withRestarts -> withOneRestart -> doWithOneRestart
Execution halted
ERROR: configuration failed for package ‘rmsb’
* removing ‘/Users/buurensv/Dropbox/Package/mice/mice/revdep/checks.noindex/rmsb/old/rmsb.Rcheck/rmsb’


```
