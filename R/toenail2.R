#' Toenail data
#' 
#' The toenail data come from a Multicenter study comparing two oral
#' treatments for toenail infection. Patients were evaluated for the
#' degree of separation of the nail. Patients were randomized into two
#' treatments and were followed over seven visits - four in the first
#' year and yearly thereafter. The patients have not been treated
#' prior to the first visit so this should be regarded as the
#' baseline.
#' @name toenail2
#' @docType data
#' @format A data frame with 1908 observations on the following 5 variables: 
#'   \describe{
#'   \item{\code{patientID}}{a numeric vector giving the ID of patient}
#'   \item{\code{outcome}}{a factor with 2 levels giving the response}
#'   \item{\code{treatment}}{a factor with 2 levels giving the treatment group}
#'   \item{\code{time}}{a numeric vector giving the time of the visit 
#'   (not exactly monthly intervals hence not round numbers)}
#'   \item{\code{visit}}{an integer giving the number of the visit}
#'   }
#' @source 
#' De Backer, M., De Vroey, C., Lesaffre, E., Scheys, I., and De
#' Keyser, P. (1998). Twelve weeks of continuous oral therapy for
#' toenail onychomycosis caused by dermatophytes: A double-blind
#' comparative trial of terbinafine 250 mg/day versus itraconazole 200
#' mg/day. Journal of the American Academy of Dermatology, 38, 57-63.
#' @references
#' Lesaffre, E. and Spiessens, B. (2001). On the effect of the number of
#' quadrature points in a logistic random-effects model: An example.
#' Journal of the Royal Statistical Society, Series C, 50, 325-335.
#' 
#' G. Fitzmaurice, N. Laird and J. Ware (2004) Applied Longitudinal Analysis, 
#' Wiley and Sons, New York, USA.
#' 
#' Van Buuren, S. (2018). 
#'\href{https://stefvanbuuren.name/fimd/sec-catoutcome.html#example}{\emph{Flexible
#'Imputation of Missing Data. Second Edition.}} Chapman & Hall/CRC.
#'Boca Raton, FL.
#' @keywords datasets
#' @seealso \code{\link{toenail}}
#' @details Apart from formatting, this dataset is identical to 
#' \code{toenail}. The formatting is taken identical to 
#' \code{data("toenail", package = "HSAUR3")}.
NULL
