context("quickpred")

test_that("returns square binary matrix", {

    predictorMatrix <- quickpred(nhanes)

    expect_is(predictorMatrix, 'matrix')
    expect_equal(nrow(predictorMatrix), ncol(predictorMatrix))
    expect_in(predictorMatrix, c(0, 1))

})

test_that("mincor supports scalar, vector, matrix", {

    n_col <- ncol(nhanes)
    expect_in(quickpred(nhanes, mincor=0), c(0, 1))
    expect_in(quickpred(nhanes, mincor=1), 0)
    expect_in(quickpred(nhanes, mincor=rep(0.1, n_col)), c(0, 1))
    expect_in(
        quickpred(nhanes, mincor=matrix(rep(0.1, n_col*n_col), ncol=n_col)),
        c(0, 1)
    )

})

test_that("minpuc supports scalar, vector, matrix", {

    n_col <- ncol(nhanes)
    expect_in(quickpred(nhanes, minpuc=0), c(0, 1))
    expect_in(quickpred(nhanes, minpuc=rep(0.1, n_col)), c(0, 1))
    expect_in(
        quickpred(nhanes, minpuc=matrix(rep(0.1, n_col*n_col), ncol=n_col)),
        c(0, 1)
    )

})

test_that("include one or more variables", {

    result_include_bmi <- quickpred(nhanes, include="bmi")
    has_missing <- apply(is.na(nhanes), 2, any)
    not_bmi <- setdiff(names(nhanes)[has_missing], "bmi")
    expect_in(result_include_bmi[not_bmi, "bmi"], 1)

    expect_in(quickpred(nhanes, include=names(nhanes)), c(0, 1))

    n_col <- ncol(nhanes)
    result_include_all <- quickpred(nhanes, include=names(nhanes))
    expect_in(
        result_include_all[has_missing, ] - (1 - diag(n_col)[has_missing,]),
        0
    )

})

test_that("exclude one or more variables", {

    result_exclude_age <- quickpred(nhanes, exclude="age")
    expect_in(result_exclude_age[, "age"], 0)

    result_exclude_all <- quickpred(nhanes, exclude=names(nhanes))
    expect_in(result_exclude_all, 0)

})
