#'Subset of Irish wind speed data
#'
#'Subset of Irish wind speed data
#'
#'The original data set is much larger and was analyzed in detail by Haslett
#'and Raftery (1989). Van Buuren et al (2006) used this subset to investigate
#'the influence of extreme MAR mechanisms on the quality of imputation.
#'
#'@name windspeed
#'@docType data
#'@format A data frame with 433 rows and 6 columns containing the daily average
#'wind speeds within the period 1961-1978 at meteorological stations in the
#'Republic of Ireland. The data are a random sample from a larger data set.
#'\describe{ 
#'\item{RochePt}{Roche Point}
#'\item{Rosslare}{Rosslare} 
#'\item{Shannon}{Shannon}
#'\item{Dublin}{Dublin} 
#'\item{Clones}{Clones}
#'\item{MalinHead}{Malin Head} }
#'@references Haslett, J. and Raftery, A. E. (1989).  \emph{Space-time
#'Modeling with Long-memory Dependence: Assessing Ireland's Wind Power
#'Resource (with Discussion)}. Applied Statistics 38, 1-50.
#'\url{http://lib.stat.cmu.edu/datasets/wind.desc} and
#'\url{http://lib.stat.cmu.edu/datasets/wind.data}
#'
#'van Buuren, S., Brand, J.P.L., Groothuis-Oudshoorn C.G.M., Rubin, D.B. (2006)
#'Fully conditional specification in multivariate imputation.  \emph{Journal of
#'Statistical Computation and Simulation}, \bold{76}, 12, 1049--1064.
#'@keywords datasets
#'@examples
#'
#'windspeed[1:3,]
#'
NULL
