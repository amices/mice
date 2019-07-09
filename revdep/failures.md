# dynr

<details>

* Version: 0.1.14-9
* Source code: https://github.com/cran/dynr
* Date/Publication: 2019-04-02 07:50:03 UTC
* Number of recursive dependencies: 97

Run `revdep_details(,"dynr")` for more info

</details>

## In both

*   checking whether package ‘dynr’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/buurensv/Package/mice/mice/revdep/checks.noindex/dynr/new/dynr.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘dynr’ ...
** package ‘dynr’ successfully unpacked and MD5 sums checked
** using staged installation
checking for gsl-config... no
configure: error: gsl-config not found, is GSL installed?
ERROR: configuration failed for package ‘dynr’
* removing ‘/Users/buurensv/Package/mice/mice/revdep/checks.noindex/dynr/new/dynr.Rcheck/dynr’

```
### CRAN

```
* installing *source* package ‘dynr’ ...
** package ‘dynr’ successfully unpacked and MD5 sums checked
** using staged installation
checking for gsl-config... no
configure: error: gsl-config not found, is GSL installed?
ERROR: configuration failed for package ‘dynr’
* removing ‘/Users/buurensv/Package/mice/mice/revdep/checks.noindex/dynr/old/dynr.Rcheck/dynr’

```
# miceFast

<details>

* Version: 0.2.3
* Source code: https://github.com/cran/miceFast
* URL: https://github.com/Polkas/miceFast
* BugReports: https://github.com/Polkas/miceFast/issues
* Date/Publication: 2018-05-06 20:19:04 UTC
* Number of recursive dependencies: 93

Run `revdep_details(,"miceFast")` for more info

</details>

## In both

*   checking whether package ‘miceFast’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/buurensv/Package/mice/mice/revdep/checks.noindex/miceFast/new/miceFast.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘miceFast’ ...
** package ‘miceFast’ successfully unpacked and MD5 sums checked
** using staged installation
** libs
clang++ -std=gnu++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/buurensv/Package/mice/mice/revdep/library.noindex/miceFast/Rcpp/include" -I"/Users/buurensv/Package/mice/mice/revdep/library.noindex/miceFast/RcppArmadillo/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include -fopenmp -fPIC  -Wall -g -O2  -c R_funs.cpp -o R_funs.o
clang: error: unsupported option '-fopenmp'
make: *** [R_funs.o] Error 1
ERROR: compilation failed for package ‘miceFast’
* removing ‘/Users/buurensv/Package/mice/mice/revdep/checks.noindex/miceFast/new/miceFast.Rcheck/miceFast’

```
### CRAN

```
* installing *source* package ‘miceFast’ ...
** package ‘miceFast’ successfully unpacked and MD5 sums checked
** using staged installation
** libs
clang++ -std=gnu++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/buurensv/Package/mice/mice/revdep/library.noindex/miceFast/Rcpp/include" -I"/Users/buurensv/Package/mice/mice/revdep/library.noindex/miceFast/RcppArmadillo/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include -fopenmp -fPIC  -Wall -g -O2  -c R_funs.cpp -o R_funs.o
clang: error: unsupported option '-fopenmp'
make: *** [R_funs.o] Error 1
ERROR: compilation failed for package ‘miceFast’
* removing ‘/Users/buurensv/Package/mice/mice/revdep/checks.noindex/miceFast/old/miceFast.Rcheck/miceFast’

```
# MissingDataGUI

<details>

* Version: 0.2-5
* Source code: https://github.com/cran/MissingDataGUI
* Date/Publication: 2016-04-25 08:58:53
* Number of recursive dependencies: 101

Run `revdep_details(,"MissingDataGUI")` for more info

</details>

## In both

*   checking whether package ‘MissingDataGUI’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/buurensv/Package/mice/mice/revdep/checks.noindex/MissingDataGUI/new/MissingDataGUI.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘MissingDataGUI’ ...
** package ‘MissingDataGUI’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
Error in dyn.load(file, DLLpath = DLLpath, ...) : 
  unable to load shared object '/Users/buurensv/Package/mice/mice/revdep/library.noindex/MissingDataGUI/RGtk2/libs/RGtk2.so':
  dlopen(/Users/buurensv/Package/mice/mice/revdep/library.noindex/MissingDataGUI/RGtk2/libs/RGtk2.so, 6): Library not loaded: /Library/Frameworks/GTK+.framework/Versions/2.24.X11/Resources/lib/libgtk-x11-2.0.0.dylib
  Referenced from: /Users/buurensv/Package/mice/mice/revdep/library.noindex/MissingDataGUI/RGtk2/libs/RGtk2.so
  Reason: image not found
Please install GTK+ from http://r.research.att.com/libs/GTK_2.24.17-X11.pkg
If the package still does not load, please ensure that GTK+ is installed and that it is on your PATH environment variable
IN ANY CASE, RESTART R BEFORE TRYING TO LOAD THE PACKAGE AGAIN
Error in dyn.load(file, DLLpath = DLLpath, ...) : 
  unable to load shared object '/Users/buurensv/Package/mice/mice/revdep/library.noindex/MissingDataGUI/cairoDevice/libs/cairoDevice.so':
  dlopen(/Users/buurensv/Package/mice/mice/revdep/library.noindex/MissingDataGUI/cairoDevice/libs/cairoDevice.so, 6): Library not loaded: /Library/Frameworks/GTK+.framework/Versions/2.24.X11/Resources/lib/libgtk-x11-2.0.0.dylib
  Referenced from: /Users/buurensv/Package/mice/mice/revdep/library.noindex/MissingDataGUI/cairoDevice/libs/cairoDevice.so
  Reason: image not found
In addition: Warning message:
Failed to load RGtk2 dynamic library, attempting to install it. 
Please install GTK+ from http://r.research.att.com/libs/GTK_2.18.5-X11.pkg
If the package still does not load, please ensure that GTK+ is installed and that it is on your PATH environment variable
IN ANY CASE, RESTART R BEFORE TRYING TO LOAD THE PACKAGE AGAIN
Error: package or namespace load failed for ‘gWidgetsRGtk2’:
 .onAttach failed in attachNamespace() for 'gWidgetsRGtk2', details:
  call: .Call(name, ..., PACKAGE = PACKAGE)
  error: "S_gtk_icon_factory_new" not available for .Call() for package "RGtk2"
Error: package ‘gWidgetsRGtk2’ could not be loaded
In addition: Warning message:
In fun(libname, pkgname) :
  Failed to load cairoDevice, attempting to install itError in dyn.load(file, DLLpath = DLLpath, ...) : 
  unable to load shared object '/Users/buurensv/Package/mice/mice/revdep/library.noindex/MissingDataGUI/cairoDevice/libs/cairoDevice.so':
  dlopen(/Users/buurensv/Package/mice/mice/revdep/library.noindex/MissingDataGUI/cairoDevice/libs/cairoDevice.so, 6): Library not loaded: /Library/Frameworks/GTK+.framework/Versions/2.24.X11/Resources/lib/libgtk-x11-2.0.0.dylib
  Referenced from: /Users/buurensv/Package/mice/mice/revdep/library.noindex/MissingDataGUI/cairoDevice/libs/cairoDevice.so
  Reason: image not found

Execution halted
ERROR: lazy loading failed for package ‘MissingDataGUI’
* removing ‘/Users/buurensv/Package/mice/mice/revdep/checks.noindex/MissingDataGUI/new/MissingDataGUI.Rcheck/MissingDataGUI’

```
### CRAN

```
* installing *source* package ‘MissingDataGUI’ ...
** package ‘MissingDataGUI’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
Error in dyn.load(file, DLLpath = DLLpath, ...) : 
  unable to load shared object '/Users/buurensv/Package/mice/mice/revdep/library.noindex/MissingDataGUI/RGtk2/libs/RGtk2.so':
  dlopen(/Users/buurensv/Package/mice/mice/revdep/library.noindex/MissingDataGUI/RGtk2/libs/RGtk2.so, 6): Library not loaded: /Library/Frameworks/GTK+.framework/Versions/2.24.X11/Resources/lib/libgtk-x11-2.0.0.dylib
  Referenced from: /Users/buurensv/Package/mice/mice/revdep/library.noindex/MissingDataGUI/RGtk2/libs/RGtk2.so
  Reason: image not found
Please install GTK+ from http://r.research.att.com/libs/GTK_2.24.17-X11.pkg
If the package still does not load, please ensure that GTK+ is installed and that it is on your PATH environment variable
IN ANY CASE, RESTART R BEFORE TRYING TO LOAD THE PACKAGE AGAIN
Error in dyn.load(file, DLLpath = DLLpath, ...) : 
  unable to load shared object '/Users/buurensv/Package/mice/mice/revdep/library.noindex/MissingDataGUI/cairoDevice/libs/cairoDevice.so':
  dlopen(/Users/buurensv/Package/mice/mice/revdep/library.noindex/MissingDataGUI/cairoDevice/libs/cairoDevice.so, 6): Library not loaded: /Library/Frameworks/GTK+.framework/Versions/2.24.X11/Resources/lib/libgtk-x11-2.0.0.dylib
  Referenced from: /Users/buurensv/Package/mice/mice/revdep/library.noindex/MissingDataGUI/cairoDevice/libs/cairoDevice.so
  Reason: image not found
In addition: Warning message:
Failed to load RGtk2 dynamic library, attempting to install it. 
Please install GTK+ from http://r.research.att.com/libs/GTK_2.18.5-X11.pkg
If the package still does not load, please ensure that GTK+ is installed and that it is on your PATH environment variable
IN ANY CASE, RESTART R BEFORE TRYING TO LOAD THE PACKAGE AGAIN
Error: package or namespace load failed for ‘gWidgetsRGtk2’:
 .onAttach failed in attachNamespace() for 'gWidgetsRGtk2', details:
  call: .Call(name, ..., PACKAGE = PACKAGE)
  error: "S_gtk_icon_factory_new" not available for .Call() for package "RGtk2"
Error: package ‘gWidgetsRGtk2’ could not be loaded
In addition: Warning message:
In fun(libname, pkgname) :
  Failed to load cairoDevice, attempting to install itError in dyn.load(file, DLLpath = DLLpath, ...) : 
  unable to load shared object '/Users/buurensv/Package/mice/mice/revdep/library.noindex/MissingDataGUI/cairoDevice/libs/cairoDevice.so':
  dlopen(/Users/buurensv/Package/mice/mice/revdep/library.noindex/MissingDataGUI/cairoDevice/libs/cairoDevice.so, 6): Library not loaded: /Library/Frameworks/GTK+.framework/Versions/2.24.X11/Resources/lib/libgtk-x11-2.0.0.dylib
  Referenced from: /Users/buurensv/Package/mice/mice/revdep/library.noindex/MissingDataGUI/cairoDevice/libs/cairoDevice.so
  Reason: image not found

Execution halted
ERROR: lazy loading failed for package ‘MissingDataGUI’
* removing ‘/Users/buurensv/Package/mice/mice/revdep/checks.noindex/MissingDataGUI/old/MissingDataGUI.Rcheck/MissingDataGUI’

```
# NNLM

<details>

* Version: 0.4.3
* Source code: https://github.com/cran/NNLM
* URL: https://github.com/linxihui/NNLM
* BugReports: https://github.com/linxihui/NNLM/issues
* Date/Publication: 2019-07-02 23:12:38 UTC
* Number of recursive dependencies: 82

Run `revdep_details(,"NNLM")` for more info

</details>

## In both

*   checking whether package ‘NNLM’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/buurensv/Package/mice/mice/revdep/checks.noindex/NNLM/new/NNLM.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘NNLM’ ...
** package ‘NNLM’ successfully unpacked and MD5 sums checked
** using staged installation
** libs
clang++ -std=gnu++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/buurensv/Package/mice/mice/revdep/library.noindex/NNLM/Rcpp/include" -I"/Users/buurensv/Package/mice/mice/revdep/library.noindex/NNLM/RcppArmadillo/include" -I"/Users/buurensv/Package/mice/mice/revdep/library.noindex/NNLM/RcppProgress/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include -fopenmp -fPIC  -Wall -g -O2  -c RcppExports.cpp -o RcppExports.o
clang: error: unsupported option '-fopenmp'
make: *** [RcppExports.o] Error 1
ERROR: compilation failed for package ‘NNLM’
* removing ‘/Users/buurensv/Package/mice/mice/revdep/checks.noindex/NNLM/new/NNLM.Rcheck/NNLM’

```
### CRAN

```
* installing *source* package ‘NNLM’ ...
** package ‘NNLM’ successfully unpacked and MD5 sums checked
** using staged installation
** libs
clang++ -std=gnu++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/buurensv/Package/mice/mice/revdep/library.noindex/NNLM/Rcpp/include" -I"/Users/buurensv/Package/mice/mice/revdep/library.noindex/NNLM/RcppArmadillo/include" -I"/Users/buurensv/Package/mice/mice/revdep/library.noindex/NNLM/RcppProgress/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include -fopenmp -fPIC  -Wall -g -O2  -c RcppExports.cpp -o RcppExports.o
clang: error: unsupported option '-fopenmp'
make: *** [RcppExports.o] Error 1
ERROR: compilation failed for package ‘NNLM’
* removing ‘/Users/buurensv/Package/mice/mice/revdep/checks.noindex/NNLM/old/NNLM.Rcheck/NNLM’

```
