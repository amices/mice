#'School data of Snijders and Bosker (2012)
#'
#'Dataset with raw data from Snijders and Bosker (2012) containing 
#'data from 4106 pupils attending 216 schools. This is the full
#'dataset including the missing data.
#'
#'@name schools
#'@docType data
#'@format \code{schools} is a data frame with 4106 rows and 14 columns: 
#'\describe{
#'\item{\code{school}}{School number}
#'\item{\code{pupil}}{Pupil ID}
#'\item{\code{iqv}}{IQ verbal}
#'\item{\code{iqp}}{IQ performal}
#'\item{\code{sex}}{Sex of pupil}
#'\item{\code{min}}{Minority 0/1}
#'\item{\code{repeatgr}}{Number of repeated class, 0, 1, 2}
#'\item{\code{langpret}}{language score before}
#'\item{\code{langpost}}{language score after}
#'\item{\code{aritpret}}{Arithmatic score before} 
#'\item{\code{aritpost}}{Arithmatic score after} 
#'\item{\code{denomi}}{Denomination classification 1-4 - at school level}
#'\item{\code{schoolses}}{SES indicator - at school level}
#'}
#'
#'@note This dataset is constructed from the raw data. There are 
#'a few differences with the data set used in Chapter 4 and 5 
#'of Snijders and Bosker:
#'\enumerate{
#'\item All schools are included, including the five school with
#'missing values on \code{langpost}.
#'\item Missing \code{denomination} codes are left as missing.
#'\item No aggregate variables at the school level are included. 
#'Aggregates are undefined in the presence of missing data
#'in the underlying values.
#'\item There is a wider selection of variables. Note however that the
#'original dataset contains additional variables.
#'}
#'
#'@source \url{https://www.stats.ox.ac.uk/~snijders/mlbook.htm}
#'
#'@keywords datasets
NULL
