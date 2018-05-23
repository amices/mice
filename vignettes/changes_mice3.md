<!-- README.md is generated from README.Rmd. Please edit that file -->
Overview
========

This documents describes the changes between `mice 2.46.0` and `mice 3.0.0`. Most code written for versions `mice 2.12 - mice 2.46.0` should run unchanged. I have tried to minimize the changes to the function arguments, but it was not possible to remain 100% backward compatible. This document outlines the visible changes, and suggests ways how to adapt old code to `mice 3.0`.

`mice` function
===============

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
<td align="left">data</td>
<td align="left"></td>
<td align="left"></td>
</tr>
<tr class="even">
<td align="left">m</td>
<td align="left"></td>
<td align="left"></td>
</tr>
<tr class="odd">
<td align="left">method</td>
<td align="left">length <code>ncol(data)</code></td>
<td align="left">length <code>length(blocks)</code></td>
</tr>
<tr class="even">
<td align="left">predictorMatrix</td>
<td align="left">square matrix: size <code>ncol(data)</code></td>
<td align="left">matrix: <code>length(blocks)</code> rows, <code>ncol(data)</code> columns</td>
</tr>
<tr class="odd">
<td align="left">where</td>
<td align="left"></td>
<td align="left"></td>
</tr>
<tr class="even">
<td align="left">blocks</td>
<td align="left">not defined</td>
<td align="left">new argument</td>
</tr>
<tr class="odd">
<td align="left">visitSequence</td>
<td align="left"><code>integer</code>, arbitrary length</td>
<td align="left"><code>character</code>, arbitrary length</td>
</tr>
<tr class="even">
<td align="left">form</td>
<td align="left">character, length <code>ncol(data)</code></td>
<td align="left">deprecated</td>
</tr>
<tr class="odd">
<td align="left">formulas</td>
<td align="left">not defined</td>
<td align="left">named list of formulas</td>
</tr>
<tr class="even">
<td align="left">blots</td>
<td align="left">not defined</td>
<td align="left">named <code>list</code> of <code>alist</code></td>
</tr>
<tr class="odd">
<td align="left">post</td>
<td align="left"></td>
<td align="left"></td>
</tr>
<tr class="even">
<td align="left">defaultMethod</td>
<td align="left"></td>
<td align="left"></td>
</tr>
<tr class="odd">
<td align="left">maxit</td>
<td align="left"></td>
<td align="left"></td>
</tr>
<tr class="even">
<td align="left">diagnostics</td>
<td align="left"></td>
<td align="left">deprecated</td>
</tr>
<tr class="odd">
<td align="left">printFlag</td>
<td align="left"></td>
<td align="left"></td>
</tr>
<tr class="even">
<td align="left">seed</td>
<td align="left"></td>
<td align="left"></td>
</tr>
<tr class="odd">
<td align="left">data.init</td>
<td align="left"></td>
<td align="left"></td>
</tr>
<tr class="even">
<td align="left">imputationMethod</td>
<td align="left"></td>
<td align="left">deprecated</td>
</tr>
<tr class="odd">
<td align="left">defaultImputationMethod</td>
<td align="left"></td>
<td align="left">deprecated</td>
</tr>
<tr class="even">
<td align="left">...</td>
<td align="left"></td>
<td align="left"></td>
</tr>
</tbody>
</table>

new functions
=============

1.  `blocks`: The main algorithm iterates over blocks. A block is simply a collection of variables. In the old MICE algorithm each block was equivalent to one variable, which - of course - is still the default; The `blocks` argument allows mixing univariate imputation method multivariate imputation methods. The `blocks` feature bridges two seemingly disparate approaches, joint modeling and fully conditional specification, into one framework;

2.  `where`: The `where` argument is a logical matrix of the same size of `data` that specifies which cells should be imputed. This opens up some new analytic possibilities (which are still to be documented);

3.  Multivariate tests: There are new functions `D1()`, `D2()`, `D3()` and `anova()` that perform multivariate parameter tests on the repeated analysis from on multiply-imputed data;

4.  `formulas`: The old `form` argument has been redesign and is now renamed to `formulas`. This provides an alternative way to specify imputation models that exploits the full power of R's native formula's.

Things on the wish list are:

1.  Better support for, and integration with, the `tidyverse` framework, especially for the packages `dplyr`, `tibble` and `broom`;

2.  Methods for automatic specification of imputation models;

3.  Easier specification of models for data that are Missing Not at Random (MNAR) and sensitivity analysis;

4.  Functionality for testing the quality of `mice.impute.xxx()` functions;

5.  Better numerical algorithms for low-level imputation function. Better handling of duplicate variables.

6.  Better documentation. Of everything...

I'll be happy to take feedback and discuss suggestions. Please submit these through Github's issues facility.

Installation
------------

The `mice` package can be installed from CRAN as follows:

``` r
install.packages("mice")
```

The latest version is can be installed from GitHub as follows:

``` r
install.packages("devtools")
devtools::install_github(repo = "stefvanbuuren/mice", ref = "dev")
```

See [MICE: Multivariate Imputation by Chained Equations](http://stefvanbuuren.github.io/mice/) for more details.
