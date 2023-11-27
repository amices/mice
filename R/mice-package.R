#' \pkg{mice}: Multivariate Imputation by Chained Equations
#'
#' The \pkg{mice} package implements a method to deal with missing data.
#' The package creates multiple imputations (replacement values) for
#' multivariate missing data. The method is based on Fully Conditional
#' Specification, where each incomplete variable is imputed by a separate
#' model. The MICE algorithm can impute mixes of continuous, binary,
#' unordered categorical and ordered categorical data. In addition, MICE
#' can impute continuous two-level data, and maintain consistency between
#' imputations by means of passive imputation. Many diagnostic plots are
#' implemented to inspect the quality of the imputations.
#'
#' The \pkg{mice} package contains functions to
#' \itemize{
#' \item Inspect the missing data pattern
#' \item Impute the missing data \emph{m} times, resulting in \emph{m} completed data sets
#' \item Diagnose the quality of the imputed values
#' \item Analyze each completed data set
#' \item Pool the results of the repeated analyses
#' \item Store and export the imputed data in various formats
#' \item Generate simulated incomplete data
#' \item Incorporate custom imputation methods
#' }
#'
#' @section Functions:
#'
#' The main functions are:
#' \tabular{ll}{
#'   \code{mice()} \tab Impute the missing data *m* times\cr
#'   \code{with()} \tab Analyze completed data sets\cr
#'   \code{pool()} \tab Combine parameter estimates\cr
#'   \code{complete()} \tab Export imputed data\cr
#'   \code{ampute()} \tab Generate missing data\cr}
#'
#' @section Vignettes:
#'
#' There is a detailed series of
#' six online vignettes that walk you through solving realistic inference
#' problems with mice.
#'
#' We suggest going through these vignettes in the following order
#' \enumerate{
#' \item \href{https://www.gerkovink.com/miceVignettes/Ad_hoc_and_mice/Ad_hoc_methods.html}{Ad hoc methods and the MICE algorithm}
#' \item \href{https://www.gerkovink.com/miceVignettes/Convergence_pooling/Convergence_and_pooling.html}{Convergence and pooling}
#' \item \href{https://www.gerkovink.com/miceVignettes/Missingness_inspection/Missingness_inspection.html}{Inspecting how the observed data and missingness are related}
#' \item \href{https://www.gerkovink.com/miceVignettes/Passive_Post_processing/Passive_imputation_post_processing.html}{Passive imputation and post-processing}
#' \item \href{https://www.gerkovink.com/miceVignettes/Multi_level/Multi_level_data.html}{Imputing multilevel data}
#' \item \href{https://www.gerkovink.com/miceVignettes/Sensitivity_analysis/Sensitivity_analysis.html}{Sensitivity analysis with \pkg{mice}}
#' }
#' Van Buuren, S. (2018).
#' Boca Raton, FL.: Chapman & Hall/CRC Press.

#' The book
#' \href{https://stefvanbuuren.name/fimd/}{\emph{Flexible Imputation of Missing Data. Second Edition.}}
#' contains a lot of \href{https://github.com/stefvanbuuren/fimdbook/tree/master/R}{example code}.
#'
#' @section Methodology:
#'
#' The \pkg{mice} software was published in the {Journal of Statistical Software} (Van Buuren and Groothuis-Oudshoorn, 2011). \doi{10.18637/jss.v045.i03}
#' The first application of the method
#' concerned missing blood pressure data (Van Buuren et. al., 1999).
#' The term \emph{Fully Conditional Specification} was introduced in 2006 to describe a general class of methods that specify imputations model for multivariate data as a set of conditional distributions (Van Buuren et. al., 2006). Further details on mixes of variables and applications can be found in the book
#' \href{https://stefvanbuuren.name/fimd/}{\emph{Flexible Imputation of Missing Data. Second Edition.}}
#' Chapman & Hall/CRC. Boca Raton, FL.
#'
#' @section Enhanced linear algebra:
#'
#' Updating the BLAS can improve speed of R, sometime considerably. The details
#' depend on the operating system. See the discussion in the
#' "R Installation and Administration" guide for further information.
#'
#' @docType package
#' @aliases mice-package
#'
#' @name mice
#' @seealso \code{\link{mice}}, \code{\link{with.mids}},
#' \code{\link{pool}}, \code{\link{complete}}, \code{\link{ampute}}
#' @references
#' van Buuren, S., Boshuizen, H.C., Knook, D.L. (1999) Multiple
#' imputation of missing blood pressure covariates in survival analysis.
#' \emph{Statistics in Medicine}, \bold{18}, 681--694.
#'
#' van Buuren, S., Brand, J.P.L., Groothuis-Oudshoorn C.G.M., Rubin, D.B. (2006)
#' Fully conditional specification in multivariate imputation.  \emph{Journal of
#' Statistical Computation and Simulation}, \bold{76}, 12, 1049--1064.
#'
#' van Buuren, S., Groothuis-Oudshoorn, K. (2011). {\code{mice}:
#' Multivariate Imputation by Chained Equations in \code{R}}. \emph{Journal of
#' Statistical Software}, \bold{45}(3), 1--67. \doi{10.18637/jss.v045.i03}
#'
#' Van Buuren, S. (2018).
#' \href{https://stefvanbuuren.name/fimd/}{\emph{Flexible Imputation of Missing Data. Second Edition.}}
#' Chapman & Hall/CRC. Boca Raton, FL.
#' @useDynLib mice, .registration = TRUE
NULL
