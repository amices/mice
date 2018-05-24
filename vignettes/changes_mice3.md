
<!-- README.md is generated from README.Rmd. Please edit that file -->
Overview
========

This documents describes the changes between `mice 2.46.0` and `mice 3.0.0`. Most code written for versions `mice 2.12 - mice 2.46.0` should run unchanged. I have tried to minimize the changes to the function arguments, but it was not possible to remain 100% backward compatible. This document outlines the visible changes, and suggests ways how to adapt old code to `mice 3.0`.

`mice` function arguments
=========================

<table style="width:92%;">
<colgroup>
<col width="16%" />
<col width="37%" />
<col width="37%" />
</colgroup>
<thead>
<tr class="header">
<th align="left">Argument</th>
<th align="left">2.46.0</th>
<th align="left">3.0.0</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td align="left">method</td>
<td align="left">length <code>ncol(data)</code></td>
<td align="left">length <code>length(blocks)</code></td>
</tr>
<tr class="even">
<td align="left">predictorMatrix</td>
<td align="left"><code>ncol(data)</code> rows and columns</td>
<td align="left"><code>length(blocks)</code> rows, <code>ncol(data)</code> columns</td>
</tr>
<tr class="odd">
<td align="left">blocks</td>
<td align="left">not defined</td>
<td align="left">new argument</td>
</tr>
<tr class="even">
<td align="left">visitSequence</td>
<td align="left"><code>integer</code>, arbitrary length</td>
<td align="left"><code>character</code>, arbitrary length</td>
</tr>
<tr class="odd">
<td align="left">form</td>
<td align="left">character, length <code>ncol(data)</code></td>
<td align="left">deprecated</td>
</tr>
<tr class="even">
<td align="left">formulas</td>
<td align="left">not defined</td>
<td align="left">named list of formulas</td>
</tr>
<tr class="odd">
<td align="left">blots</td>
<td align="left">not defined</td>
<td align="left">named <code>list</code> of <code>alist</code></td>
</tr>
<tr class="even">
<td align="left">diagnostics</td>
<td align="left"></td>
<td align="left">deprecated</td>
</tr>
<tr class="odd">
<td align="left">imputationMethod</td>
<td align="left"></td>
<td align="left">deprecated, use <code>method</code></td>
</tr>
<tr class="even">
<td align="left">defaultImputationMethod</td>
<td align="left"></td>
<td align="left">deprecated, use <code>defaultMethod</code></td>
</tr>
<tr class="odd">
<td align="left">...</td>
<td align="left"></td>
<td align="left"></td>
</tr>
</tbody>
</table>

No changes were made for the following arguments: `data`, `m`, `where`, `post`, `defaultMethod`, `maxit`, `printFlag`, `seed` and `data.init`.

If `blocks` is not specified, then each variable is allocated to a separate block. In that case, `length(blocks)` is identical to `ncol(data)`, and the method in `mice 3.0.0` reduces to variable-by-variable imputation, as in `mice 2.46.0` and before.

Argument `visitSequence` may still be specified as `integer` or `numeric`, but it will internally be converted into `character` using the column names in `data`.

An existing function call to `mice` using the old `form` argument may result in an error `Argument "formulas" not a list`. The advice is to specify the formula as a list, e.g.,

``` r
library(mice, warn.conflicts = FALSE)
imp <- mice(nhanes, 
            formulas = list(hyp ~ bmi, 
                            chl ~ age + hyp + bmi,
                            bmi ~ age + hyp + chl),
            print = FALSE, m = 1, maxit = 1, seed = 1)
imp$formulas
#> $hyp
#> hyp ~ bmi
#> 
#> $chl
#> chl ~ age + hyp + bmi
#> 
#> $bmi
#> bmi ~ age + hyp + chl
```
