# brms (2.23.0)

* GitHub: <https://github.com/paul-buerkner/brms>
* Email: <mailto:paul.buerkner@gmail.com>
* GitHub mirror: <https://github.com/cran/brms>

Run `revdepcheck::revdep_details(, "brms")` for more info

## In both

*   R CMD check timed out


# dynr (0.1.16-114)

* GitHub: <https://github.com/mhunter1/dynr>
* Email: <mailto:mike.dynr@gmail.com>
* GitHub mirror: <https://github.com/cran/dynr>

Run `revdepcheck::revdep_details(, "dynr")` for more info

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
** this is package ‘dynr’ version ‘0.1.16-114’
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
** this is package ‘dynr’ version ‘0.1.16-114’
** package ‘dynr’ successfully unpacked and MD5 sums checked
** using staged installation
checking for gsl-config... no
configure: error: gsl-config not found, is GSL installed?
ERROR: configuration failed for package ‘dynr’
* removing ‘/Users/buurensv/Dropbox/Package/mice/mice/revdep/checks.noindex/dynr/old/dynr.Rcheck/dynr’


```
# idem (5.2)

* GitHub: <https://github.com/olssol/idem>
* Email: <mailto:cwang68@jhmi.edu>
* GitHub mirror: <https://github.com/cran/idem>

Run `revdepcheck::revdep_details(, "idem")` for more info

## In both

*   checking whether package ‘idem’ can be installed ... ERROR
     ```
     Installation failed.
     See ‘/Users/buurensv/Dropbox/Package/mice/mice/revdep/checks.noindex/idem/new/idem.Rcheck/00install.out’ for details.
     ```

## Installation

### Devel

```
* installing *source* package ‘idem’ ...
** this is package ‘idem’ version ‘5.2’
** package ‘idem’ successfully unpacked and MD5 sums checked
** using staged installation
Error in rstan::stanc(file_name, allow_undefined = TRUE, obfuscate_model_name = FALSE,  : 
  0
Syntax error in 'string', line 17, column 29 to column 30, parsing error:
   -------------------------------------------------
    15:    }
    16:    real cond_lpdf(vector ymis, vector yobs, matrix coef,
...

Ill-formed type. Expected an unsized type in function signature.
  For example, this means a type like "matrix", not "matrix[3,4]".
  If you are trying to use the old array syntax, you will need to use
  the new syntax, see:
  https://mc-stan.org/docs/reference-manual/types.html#array-data-types.section
Calls: <Anonymous> -> sapply -> lapply -> FUN -> <Anonymous>
Execution halted
ERROR: configuration failed for package ‘idem’
* removing ‘/Users/buurensv/Dropbox/Package/mice/mice/revdep/checks.noindex/idem/new/idem.Rcheck/idem’


```
### CRAN

```
* installing *source* package ‘idem’ ...
** this is package ‘idem’ version ‘5.2’
** package ‘idem’ successfully unpacked and MD5 sums checked
** using staged installation
Error in rstan::stanc(file_name, allow_undefined = TRUE, obfuscate_model_name = FALSE,  : 
  0
Syntax error in 'string', line 17, column 29 to column 30, parsing error:
   -------------------------------------------------
    15:    }
    16:    real cond_lpdf(vector ymis, vector yobs, matrix coef,
...

Ill-formed type. Expected an unsized type in function signature.
  For example, this means a type like "matrix", not "matrix[3,4]".
  If you are trying to use the old array syntax, you will need to use
  the new syntax, see:
  https://mc-stan.org/docs/reference-manual/types.html#array-data-types.section
Calls: <Anonymous> -> sapply -> lapply -> FUN -> <Anonymous>
Execution halted
ERROR: configuration failed for package ‘idem’
* removing ‘/Users/buurensv/Dropbox/Package/mice/mice/revdep/checks.noindex/idem/old/idem.Rcheck/idem’


```
# pguIMP (0.1.1)

* GitHub: <https://github.com/JornLotsch/pguIMP>
* Email: <mailto:j.lotsch@em.uni-frankfurt.de>
* GitHub mirror: <https://github.com/cran/pguIMP>

Run `revdepcheck::revdep_details(, "pguIMP")` for more info

## In both

*   checking whether package ‘pguIMP’ can be installed ... ERROR
     ```
     Installation failed.
     See ‘/Users/buurensv/Dropbox/Package/mice/mice/revdep/checks.noindex/pguIMP/new/pguIMP.Rcheck/00install.out’ for details.
     ```

## Installation

### Devel

```
* installing *source* package ‘pguIMP’ ...
** this is package ‘pguIMP’ version ‘0.1.1’
** package ‘pguIMP’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** inst
** byte-compile and prepare package for lazy loading
Error: .onLoad failed in loadNamespace() for 'rJava', details:
  call: dyn.load(jli, FALSE)
  error: unable to load shared object '/Library/Java/JavaVirtualMachines/jdk1.8.0_144.jdk/Contents/Home/jre/lib/jli/libjli.dylib':
  dlopen(/Library/Java/JavaVirtualMachines/jdk1.8.0_144.jdk/Contents/Home/jre/lib/jli/libjli.dylib, 0x000A): tried: '/Library/Java/JavaVirtualMachines/jdk1.8.0_144.jdk/Contents/Home/jre/lib/jli/libjli.dylib' (mach-o file, but is an incompatible architecture (have 'x86_64', need 'arm64e' or 'arm64e.v1' or 'arm64' or 'arm64')), '/System/Volumes/Preboot/Cryptexes/OS/Library/Java/JavaVirtualMachines/jdk1.8.0_144.jdk/Contents/Home/jre/lib/jli/libjli.dylib' (no such file), '/Library/Java/JavaVirtualMachines/jdk1.8.0_144.jdk/Contents/Home/jre/lib/jli/libjli.dylib' (mach-o file, but is an incompatible architecture (have 'x86_64', need 'arm64e' or 'arm64e.v1' or 'arm64' or 'arm64'))
Execution halted
ERROR: lazy loading failed for package ‘pguIMP’
* removing ‘/Users/buurensv/Dropbox/Package/mice/mice/revdep/checks.noindex/pguIMP/new/pguIMP.Rcheck/pguIMP’


```
### CRAN

```
* installing *source* package ‘pguIMP’ ...
** this is package ‘pguIMP’ version ‘0.1.1’
** package ‘pguIMP’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** inst
** byte-compile and prepare package for lazy loading
Error: .onLoad failed in loadNamespace() for 'rJava', details:
  call: dyn.load(jli, FALSE)
  error: unable to load shared object '/Library/Java/JavaVirtualMachines/jdk1.8.0_144.jdk/Contents/Home/jre/lib/jli/libjli.dylib':
  dlopen(/Library/Java/JavaVirtualMachines/jdk1.8.0_144.jdk/Contents/Home/jre/lib/jli/libjli.dylib, 0x000A): tried: '/Library/Java/JavaVirtualMachines/jdk1.8.0_144.jdk/Contents/Home/jre/lib/jli/libjli.dylib' (mach-o file, but is an incompatible architecture (have 'x86_64', need 'arm64e' or 'arm64e.v1' or 'arm64' or 'arm64')), '/System/Volumes/Preboot/Cryptexes/OS/Library/Java/JavaVirtualMachines/jdk1.8.0_144.jdk/Contents/Home/jre/lib/jli/libjli.dylib' (no such file), '/Library/Java/JavaVirtualMachines/jdk1.8.0_144.jdk/Contents/Home/jre/lib/jli/libjli.dylib' (mach-o file, but is an incompatible architecture (have 'x86_64', need 'arm64e' or 'arm64e.v1' or 'arm64' or 'arm64'))
Execution halted
ERROR: lazy loading failed for package ‘pguIMP’
* removing ‘/Users/buurensv/Dropbox/Package/mice/mice/revdep/checks.noindex/pguIMP/old/pguIMP.Rcheck/pguIMP’


```
# rmsb (1.1-3)

* Email: <mailto:fh@fharrell.com>
* GitHub mirror: <https://github.com/cran/rmsb>

Run `revdepcheck::revdep_details(, "rmsb")` for more info

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
** this is package ‘rmsb’ version ‘1.1-3’
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
** this is package ‘rmsb’ version ‘1.1-3’
** package ‘rmsb’ successfully unpacked and MD5 sums checked
** using staged installation
Error in loadNamespace(x) : there is no package called ‘rstantools’
Calls: loadNamespace -> withRestarts -> withOneRestart -> doWithOneRestart
Execution halted
ERROR: configuration failed for package ‘rmsb’
* removing ‘/Users/buurensv/Dropbox/Package/mice/mice/revdep/checks.noindex/rmsb/old/rmsb.Rcheck/rmsb’


```
# UKBAnalytica ()

* GitHub: <https://github.com/amices/mice>
* Email: <mailto:stef.vanbuuren@tno.nl>

Run `revdepcheck::revdep_details(, "UKBAnalytica")` for more info

## Error before installation

### Devel

```

  There are binary versions available but the source versions are later:
           binary source needs_compilation
enrichplot 1.32.0 1.32.1             FALSE
tidytree    0.4.8  0.4.9             FALSE



installing the source packages ‘enrichplot’, ‘GO.db’, ‘org.Hs.eg.db’, ‘tidytree’

Error in (function (libdir, packages, quiet, repos)  : 
  all(packages %in% rownames(installed.packages(libdir[1]))) is not TRUE
In addition: Warning messages:
1: In utils::install.packages(pkgs = pkgs, lib = lib, repos = myrepos,  :
  installation of package ‘enrichplot’ had non-zero exit status
2: In utils::install.packages(pkgs = pkgs, lib = lib, repos = myrepos,  :
  installation of package ‘enrichplot’ had non-zero exit status


```
### CRAN

```

  There are binary versions available but the source versions are later:
           binary source needs_compilation
enrichplot 1.32.0 1.32.1             FALSE
tidytree    0.4.8  0.4.9             FALSE



installing the source packages ‘enrichplot’, ‘GO.db’, ‘org.Hs.eg.db’, ‘tidytree’

Error in (function (libdir, packages, quiet, repos)  : 
  all(packages %in% rownames(installed.packages(libdir[1]))) is not TRUE
In addition: Warning messages:
1: In utils::install.packages(pkgs = pkgs, lib = lib, repos = myrepos,  :
  installation of package ‘enrichplot’ had non-zero exit status
2: In utils::install.packages(pkgs = pkgs, lib = lib, repos = myrepos,  :
  installation of package ‘enrichplot’ had non-zero exit status


```
