cran-comments
================

Package built by

``` r
library("devtools")
build_vignettes()
build()
```

`"/Users/buurensv/Package/mice/mice_3.0.0.tar.gz"`

Test environments
-----------------

-   local OS X install, 10.13.4, R 3.5.0
-   win-builder, using `devtools::build_win()`

There was no ERROR, WARNING or NOTE.

R CMD check results
-------------------

local checks of tarball fails:

`$ R CMD CHECK mice_3.0.0.tar.gz`

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

There were 48 reverse dependencies.

Packages with problems + actions

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
<td align="left">no error in brms 2.3.1 (SOLVED)</td>
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
<td align="left">solved by change in md.pattern()</td>
</tr>
<tr class="even">
<td align="left">dlookr</td>
<td align="left">0.3.0</td>
<td align="right">1</td>
<td align="right">0</td>
<td align="right">0</td>
<td align="left">author noted, not related to mice (SOLVED)</td>
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
<td align="left">solved by reverting print.mira, broom naming, author noted</td>
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
<td align="left"><a href="https://github.com/alexanderrobitzsch/miceadds/issues/6" class="uri">https://github.com/alexanderrobitzsch/miceadds/issues/6</a>, will be solved in next release</td>
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
<td align="left">fortran compilation errors, author noted</td>
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
<td align="left">no errors found in sjmisc 2.7.2 (SOLVED)</td>
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

See <https://github.com/stefvanbuuren/mice/blob/master/revdep/problems.md>
