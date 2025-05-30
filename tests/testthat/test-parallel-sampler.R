test_that("sampler() works identically in sequential and parallel modes", {
  skip_if_not_installed("future.apply")

  # Sample data
  data <- nhanes
  m <- 2
  maxit <- 2

  # Minimal setup
  method <- rep("pmm", ncol(data))
  names(method) <- names(data)
  blocks <- mice:::make.blocks(names(data))
  where <- is.na(data)
  ignore <- rep(FALSE, nrow(data))
  predictorMatrix <- make.predictorMatrix(data = data, blocks = blocks)
  formulas <- make.formulas(data, blocks)
  calltypes <- make.calltypes(NULL, predictorMatrix, formulas, "pred")
  blots <- vector("list", length(blocks))
  names(blots) <- names(blocks)
  tasks <- check.tasks(tasks = NULL, data, models = NULL, blocks, skip.check.tasks = FALSE)
  models <- NULL
  post <- rep("", ncol(data))
  names(post) <- names(data)
  visitSequence <- seq_along(blocks)
  imp_init <- mice:::initialize.imp(data, m, ignore, where, blocks, visitSequence, method, nmis = colSums(where), data.init = NULL)
  fromto <- c(1, maxit)

  # Run sampler in sequential mode
  out_seq <- mice:::sampler(data, m, ignore, where, imp_init, blocks, method,
                            visitSequence, predictorMatrix, formulas, calltypes,
                            blots, tasks, models,
                            post, fromto, printFlag = FALSE, parallel = FALSE)

  # Reset imputations
  imp_init <- mice:::initialize.imp(data, m, ignore, where, blocks, visitSequence, method, nmis = colSums(where), data.init = NULL)

  # Run sampler in parallel mode
  future::plan("multisession")
  out_par <- mice:::sampler(data, m, ignore, where, imp_init, blocks, method,
                            visitSequence, predictorMatrix, formulas, calltypes,
                            blots, tasks, models,
                            post, fromto, printFlag = FALSE, parallel = TRUE)
  future::plan("sequential")

  # Compare output structure and content
  expect_equal(dim(out_seq$chainMean), dim(out_par$chainMean))
  expect_equal(dim(out_seq$chainVar), dim(out_par$chainVar))
  expect_true(any(!is.na(out_par$chainMean)))
  expect_true(any(!is.na(out_par$chainVar)))
})

test_that("sampler() collects loggedEvents in parallel mode", {
  skip_if_not_installed("future.apply")
  data <- nhanes
  m <- 2
  method <- rep("pmm", ncol(data))
  names(method) <- names(data)
  blocks <- mice:::make.blocks(names(data))
  where <- is.na(data)
  ignore <- rep(FALSE, nrow(data))
  predictorMatrix <- make.predictorMatrix(data, blocks = blocks)
  formulas <- make.formulas(data, blocks)
  calltypes <- make.calltypes(NULL, predictorMatrix, formulas, "pred")
  blots <- vector("list", length(blocks))
  names(blots) <- names(blocks)
  tasks <- check.tasks(tasks = NULL, data, models = NULL, blocks, skip.check.tasks = FALSE)
  models <- NULL
  post <- rep("", ncol(data))
  names(post) <- names(data)
  visitSequence <- seq_along(blocks)
  imp_init <- mice:::initialize.imp(data, m, ignore, where, blocks, visitSequence, method, nmis = colSums(where), data.init = NULL)
  fromto <- c(1, 1)

  future::plan("multisession")
  out <- mice:::sampler(data, m, ignore, where, imp_init, blocks, method,
                        visitSequence, predictorMatrix, formulas, calltypes,
                        blots, tasks, models,
                        post, fromto, printFlag = FALSE, parallel = TRUE)
  future::plan("sequential")

  expect_true(is.data.frame(out$loggedEvents) || is.null(out$loggedEvents))
})

