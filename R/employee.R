#' Employee selection data
#'
#' A toy example from Craig Enders.
#'
#' Enders describes these data as follows:
#' I designed these data to mimic an employee selection scenario in
#' which prospective employees complete an IQ test and a
#' psychological well-being questionnaire during their interview.
#' The company subsequently hires the applications that score in the
#' upper half of the IQ distribution, and a supervisor rates their
#' job performance following a 6-month probationary period.
#' Note that the job performance scores are missing at random (MAR)
#' (i.e. individuals in the lower half of the IQ distribution were
#' never hired, and thus have no performance rating). In addition,
#' I randomly deleted three of the well-being scores in order to
#' mimic a situation where the applicant's well-being questionnaire
#' is inadvertently lost.
#'
#' A larger version of this data set in present as
#' \code{\link[miceadds:data.enders]{data.enders.employee}}.
#'
#' @format A data frame with 20 rows and 3 variables:
#' \describe{
#' \item{IQ}{candidate IQ score}
#' \item{wbeing}{candidate well-being score}
#' \item{jobperf}{candidate job performance score}
#' }
#' @source Enders (2010), Applied Missing Data Analysis, p. 218
"employee"
