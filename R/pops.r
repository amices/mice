#'Project on preterm and small for gestational age infants (POPS)
#'
#'Subset of data from the POPS study, a national, prospective study on preterm
#'children, including all liveborn infants <32 weeks gestional age and/or <1500
#'g from 1983 (n = 1338).
#'
#'The data set concerns of subset of 959 children that survived up to the age
#'of 19 years.
#'
#'Hille et al (2005) divided the 959 survivors into three groups: Full
#'responders (examined at an outpatient clinic and completed the
#'questionnaires, n = 596), postal responders (only completed the mailed
#'questionnaires, n = 109), non-responders (did not respond to any of the
#'mailed requests or telephone calls, or could not be traced, n = 254).
#'
#'Compared to the postal and non-responders, the full response group consists
#'of more girls, contains more Dutch children, has higher educational and
#'social economic levels and has fewer handicaps. The responders form a highly
#'selective subgroup in the total cohort.
#'
#'Multiple imputation of this data set has been described in Hille et al (2007)
#'and Van Buuren (2012), chapter 8.
#'
#'@name pops
#'@aliases pops pops.pred
#'@docType data
#'@format \code{pops} is a data frame with 959 rows and 86 columns.
#'\code{pops.pred} is the 86 by 86 binary predictor matrix used for specifying
#'the multiple imputation model.
#'@source
#'
#'Hille, E. T. M., Elbertse, L., Bennebroek Gravenhorst, J., Brand, R.,
#'Verloove-Vanhorick, S. P. (2005).  Nonresponse bias in a follow-up study of
#'19-year-old adolescents born as preterm infants. Pediatrics, 116(5):662666.
#'
#'Hille, E. T. M., Weisglas-Kuperus, N., Van Goudoever, J. B., Jacobusse, G.
#'W., Ens-Dokkum, M. H., De Groot, L., Wit, J. M., Geven, W. B., Kok, J. H., De
#'Kleine, M. J. K., Kollee, L. A. A., Mulder, A. L. M., Van Straaten, H. L. M.,
#'De Vries, L. S., Van Weissenbruch, M. M., Verloove-Vanhorick, S. P. (2007).
#'Functional outcomes and participation in young adulthood for very preterm and
#'very low birth weight infants: The Dutch project on preterm and small for
#'gestational age infants at 19 years of age. Pediatrics, 120(3):587595.
#'
#'van Buuren, S. (2012). \emph{Flexible Imputation of Missing Data.} Boca
#'Raton, FL: Chapman & Hall/CRC Press.
#'@keywords datasets
#'@examples
#'
#'
#'pops <- data(pops)
#'
NULL

