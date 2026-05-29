#' Imputation by a two-level logistic model using \code{glmer}
#'
#' Imputes univariate systematically and sporadically missing data
#' using a two-level logistic model using \code{lme4::glmer()}
#'
#' Data are missing systematically if they have not been measured, e.g., in the
#' case where we combine data from different sources. Data are missing sporadically
#' if they have been partially observed.
#'
#' The \code{random.effects} argument controls how the cluster-specific random
#' effect \eqn{b_i^*} is drawn for clusters that have missing values:
#'
#' \describe{
#'   \item{\code{"laplace"}}{(default) For clusters with observed data, draws
#'     \eqn{b_i^*} from \eqn{N(m_i, V_i)} where \eqn{m_i} is the posterior
#'     mode (BLUP) and \eqn{V_i} is the Laplace-approximated posterior
#'     variance returned by \code{lme4::ranef(fit, condVar = TRUE)}.
#'     For clusters with no observed data, draws from the marginal prior
#'     \eqn{N(0, \psi^*)}. This is the method recommended by Audigier et al.
#'     (2018).}
#'   \item{\code{"eb"}}{Empirical Bayes: for clusters with observed data, uses
#'     the BLUP as the mean but draws with the full marginal variance
#'     \eqn{\psi^*}, ignoring posterior uncertainty in \eqn{b_i}.}
#'   \item{\code{"marginal"}}{Draws all \eqn{b_i^*} from the marginal prior
#'     \eqn{N(0, \psi^*)} regardless of whether the cluster has observed data.
#'     This was the original behaviour and is correct only for systematic
#'     missingness.}
#' }
#'
#' @inheritParams mice.impute.2l.lmer
#' @param intercept Logical determining whether the intercept is automatically
#'   added.
#' @param random.effects Character string specifying how the random effect is
#'   drawn for sporadic clusters. One of \code{"laplace"} (default),
#'   \code{"eb"}, or \code{"marginal"}. See Details.
#' @param \dots Arguments passed down to \code{glmer}
#' @return Vector with imputed data, same type as \code{y}, and of length
#'   \code{sum(wy)}
#' @author Shahab Jolani, 2015; adapted to mice, SvB, 2018;
#'   random.effects argument, SvB, 2026
#' @references
#' Audigier, V., White, I. R., Jolani, S., Debray, T. P. A., Quartagno, M.,
#' Carpenter, J., van Buuren, S., & Resche-Rigon, M. (2018).
#' Multiple imputation for multilevel data with continuous and binary
#' variables. \emph{Statistical Science}, 33(2), 160-183.
#' \doi{10.1214/18-STS646}
#'
#' Jolani S., Debray T.P.A., Koffijberg H., van Buuren S., Moons K.G.M. (2015).
#' Imputation of systematically missing predictors in an individual
#' participant data meta-analysis: a generalized approach using MICE.
#' \emph{Statistics in Medicine}, 34:1841-1863.
#' @family univariate-2l
#' @keywords datagen
#' @examples
#' library(tidyr)
#' library(dplyr)
#' data("toenail2")
#' data <- tidyr::complete(toenail2, patientID, visit) %>%
#'   tidyr::fill(treatment) %>%
#'   dplyr::select(-time) %>%
#'   dplyr::mutate(patientID = as.integer(patientID))
#' \dontrun{
#' pred <- mice(data, print = FALSE, maxit = 0, seed = 1)$pred
#' pred["outcome", "patientID"] <- -2
#' imp <- mice(data, method = "2l.bin", pred = pred, maxit = 1, m = 1, seed = 1)
#' imp_laplace <- mice(data, method = "2l.bin", pred = pred, maxit = 1, m = 1,
#'                     seed = 1, random.effects = "laplace")
#' imp_eb <- mice(data, method = "2l.bin", pred = pred, maxit = 1, m = 1,
#'                seed = 1, random.effects = "eb")
#' imp_marginal <- mice(data, method = "2l.bin", pred = pred, maxit = 1, m = 1,
#'                      seed = 1, random.effects = "marginal")
#' }
#' @export
mice.impute.2l.bin <- function(
  y,
  ry,
  x,
  type,
  wy = NULL,
  intercept = TRUE,
  random.effects = c("laplace", "eb", "marginal"),
  ...
) {
  install.on.demand("lme4", ...)
  random.effects <- match.arg(random.effects)

  if (is.null(wy)) {
    wy <- !ry
  }
  if (intercept) {
    x <- cbind(1, as.matrix(x))
    type <- c(2, type)
    names(type)[1] <- colnames(x)[1] <- "(Intercept)"
  }

  clust <- names(type[type == -2])
  rande <- names(type[type == 2])
  fixe <- names(type[type > 0])

  X <- x[, fixe, drop = FALSE]
  Z <- x[, rande, drop = FALSE]
  xobs <- x[ry, , drop = FALSE]
  yobs <- y[ry]

  # create formula, use [-1] to remove intercept
  fr <- ifelse(
    length(rande) > 1,
    paste("+ ( 1 +", paste(rande[-1L], collapse = "+")),
    "+ ( 1 "
  )
  randmodel <- paste(
    "yobs ~ ",
    paste(fixe[-1L], collapse = "+"),
    fr,
    "|",
    clust,
    ")"
  )

  suppressWarnings(
    fit <- try(
      lme4::glmer(
        formula(randmodel),
        data = data.frame(yobs, xobs),
        family = binomial,
        ...
      ),
      silent = TRUE
    )
  )
  if (!is.null(attr(fit, "class"))) {
    if (attr(fit, "class") == "try-error") {
      warning("glmer does not run. Simplify imputation model")
      return(y[wy])
    }
  }

  # draw beta*
  beta <- lme4::fixef(fit)
  rv <- t(chol(vcov(fit)))
  beta.star <- beta + rv %*% rnorm(ncol(rv))

  # calculate psi*
  psi.hat <- matrix(
    lme4::VarCorr(fit)[[1L]],
    nrow = dim(lme4::VarCorr(fit)[[1L]])[1L]
  )
  s <- nrow(psi.hat) * psi.hat
  re <- lme4::ranef(fit, condVar = random.effects == "laplace")
  rancoef <- as.matrix(re[[1L]])
  lambda <- t(rancoef) %*% rancoef
  temp <- lambda + s
  if (attr(suppressWarnings(chol(temp, pivot = TRUE)), "rank") != nrow(temp)) {
    warning("The cov matrix is not full rank")
  }
  temp <- MASS::ginv(temp)
  ev <- eigen(temp)
  if (mode(ev$values) == "complex") {
    ev$values <- suppressWarnings(as.numeric(ev$values))
    ev$vectors <- suppressWarnings(matrix(
      as.numeric(ev$vectors),
      nrow = length(ev$values)
    ))
    warning("The cov matrix is complex")
  }
  if (sum(ev$values < 0) > 0) {
    ev$values[ev$values < 0] <- 0
    temp <- ev$vectors %*%
      diag(ev$values, nrow = length(ev$values)) %*%
      t(ev$vectors)
  }
  deco <- ev$vectors %*% diag(sqrt(ev$values), nrow = length(ev$values))
  temp.psi.star <- stats::rWishart(
    1,
    nrow(rancoef) + nrow(psi.hat),
    diag(nrow(psi.hat))
  )[,, 1L]
  psi.star <- MASS::ginv(deco %*% temp.psi.star %*% t(deco))

  # psi.star positive definite?
  if (!isSymmetric(psi.star)) {
    psi.star <- (psi.star + t(psi.star)) / 2
  }
  valprop <- eigen(psi.star)
  if (sum(valprop$values < 0) > 0) {
    valprop$values[valprop$values < 0] <- 0
    psi.star <- valprop$vectors %*% diag(valprop$values) %*% t(valprop$vectors)
  }

  # clusters needing imputation, split into sporadic and systematic
  clmis <- unique(x[wy, clust])
  clobs <- unique(xobs[, clust])          # clusters with observed y
  clmis_sporadic   <- clmis[clmis %in% clobs]
  clmis_systematic <- clmis[!clmis %in% clobs]

  # Laplace posterior variance array (q x q x n_clusters), NULL otherwise
  postVar <- if (random.effects == "laplace") attr(re[[1L]], "postVar") else NULL

  # draw bi.star per cluster, collect in a named list
  draw_bi <- function(cluster_id) {
    if (cluster_id %in% clmis_systematic || random.effects == "marginal") {
      # systematic: draw from marginal prior N(0, psi.star)
      myi <- matrix(0, nrow = nrow(psi.star), ncol = 1)
      vyi <- psi.star
    } else if (random.effects == "eb") {
      # empirical Bayes: centre on BLUP, spread with marginal variance
      myi <- matrix(rancoef[rownames(rancoef) == cluster_id, ], ncol = 1)
      vyi <- psi.star
    } else {
      # laplace: centre on BLUP, spread with Laplace posterior variance,
      # scaled from psi.hat to psi.star so the draw is coherent with beta.star
      ri    <- which(rownames(rancoef) == cluster_id)
      myi   <- matrix(rancoef[ri, ], ncol = 1)
      scale <- if (all(psi.hat == 0)) 1 else mean(diag(psi.star)) / mean(diag(psi.hat))
      vyi   <- matrix(postVar[,, ri], nrow = nrow(psi.star)) * scale
    }
    # symmetrise and floor negative eigenvalues (mirrors 2l.lmer)
    vyi <- vyi - upper.tri(vyi) * vyi + t(lower.tri(vyi) * vyi)
    deco1 <- eigen(vyi)
    if (sum(deco1$values > 0) == length(deco1$values)) {
      A <- deco1$vectors %*% sqrt(diag(deco1$values, nrow = length(deco1$values)))
      myi + A %*% rnorm(length(myi))
    } else {
      deco1 <- try(svd(vyi), silent = TRUE)
      if (!inherits(deco1, "try-error")) {
        A <- deco1$u %*% sqrt(diag(deco1$d, nrow = length(deco1$d)))
        myi + A %*% rnorm(length(myi))
      } else {
        warning("b_", cluster_id, " fixed to estimate")
        myi
      }
    }
  }

  # draw all bi.star and stack into matrix (n_clmis x q)
  B <- do.call(rbind, lapply(clmis, draw_bi))
  if (nrow(psi.star) == 1L) B <- matrix(B, ncol = 1L)

  clust_idx <- match(x[wy, clust], clmis)
  logit <- X[wy, , drop = FALSE] %*% beta.star +
    rowSums(Z[wy, , drop = FALSE] * B[clust_idx, , drop = FALSE])
  vec <- rbinom(sum(wy), 1, as.vector(1 / (1 + exp(-logit))))
  if (is.factor(y)) {
    vec <- factor(vec, c(0, 1), levels(y))
  }
  vec
}
