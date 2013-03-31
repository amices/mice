\name{cbind.mids}
\alias{cbind.mids}
\title{Columnwise combination of a \code{mids} object.}
\usage{
  cbind.mids(x, y, ...)
}
\arguments{
  \item{x}{A \code{mids} object.}

  \item{y}{A \code{mids} object or a \code{data.frame},
  \code{matrix}, \code{factor} or \code{vector}.}

  \item{\dots}{Additional \code{data.frame}, \code{matrix},
  \code{vector} or \code{factor}. These can be given as
  named arguments.}
}
\value{
  An S3 object of class \code{mids}
}
\description{
  This function combines two \code{mids} objects columnwise
  into a single object of class \code{mids}, or combines a
  \code{mids} object with a \code{vector}, \code{matrix},
  \code{factor} or \code{data.frame} columnwise into an
  object of class \code{mids}. The number of rows in the
  (incomplete) data \code{x$data} and \code{y} (or
  \code{y$data} if \code{y} is a \code{mids} object) should
  be equal. If \code{y} is a \code{mids} object then the
  number of imputations in \code{x} and \code{y} should be
  equal. Note: If \code{y} is a vector or factor its
  original name is lost and it will be denoted with
  \code{y} in the \code{mids} object.
}
\note{
  Component \code{call} is a vector, with first argument
  the \code{mice()} statement that created \code{x} and
  second argument the call to \code{cbind.mids()}.
  Component \code{data} is the code{cbind} of the
  (incomplete) data in \code{x$data} and \code{y$data}.
  Component \code{m} is the number of imputations.
  Component \code{nmis} is an array containing the number
  of missing observations per column. Component \code{imp}
  is a list of \code{nvar} components with the generated
  multiple imputations.  Each part of the list is a
  \code{nmis[j]} by \code{m} matrix of imputed values for
  variable \code{j}. The original data of \code{y} will be
  copied into this list, including the missing values of
  \code{y} then \code{y} is not imputed. Component
  \code{method} is a vector of strings of
  \code{length(nvar)} specifying the elementary imputation
  method per column. If \code{y} is a \code{mids} object
  this vector is a combination of \code{x$method} and
  \code{y$method}, otherwise this vector is \code{x$method}
  and for the columns of \code{y} the method is set to
  \code{''}. Component \code{predictorMatrix} is a square
  matrix of size \code{ncol(data)} containing integer data
  specifying the predictor set. If \code{x} and \code{y}
  are \code{mids} objects then the predictor matrices of
  \code{x} and \code{y} are combined with zero matrices on
  the off-diagonal blocks. Otherwise the variables in
  \code{y} are included in the predictor matrix of \code{x}
  such that \code{y} is not used as predictor(s) and not
  imputed as well. Component \code{visitSequence} is the
  sequence in which columns are visited. The same as
  \code{x$visitSequence}. Component \code{seed} is the seed
  value of the solution, \code{x$seed}. Component
  \code{iteration} is the last Gibbs sampling iteration
  number, \code{x$iteration}. Component
  \code{lastSeedValue} is the most recent seed value,
  \code{x$lastSeedValue} Component \code{chainMean} is the
  combination of \code{x$chainMean} and \code{y$chainMean}.
  If \code{y$chainMean} does not exist this element equals
  \code{x$chainMean}. Component \code{chainVar} is the
  combination of \code{x$chainVar} and \code{y$chainVar}.
  If \code{y$chainVar} does not exist this element equals
  \code{x$chainVar}. Component \code{pad} is a list
  containing various settings of the padded imputation
  model, i.e. the imputation model after creating dummy
  variables.  This list is defined by combining
  \code{x$pad} and \code{y$pad} if \code{y} is a
  \code{mids} object. Otherwise, it is defined by the
  settings of \code{x} and the combination of the data
  \code{x$data} and \code{y}. Component \code{loggedEvents}
  is set to \code{x$loggedEvents}. If a column of \code{y}
  is categorical this is ignored in the padded model since
  that column is not used as predictor for another column.
}
\examples{
# append 'forgotten' variable bmi to imp
temp <- boys[,c(1:3,5:9)]
imp  <- mice(temp,maxit=1,m=2)
imp2 <- cbind.mids(imp, data.frame(bmi=boys$bmi))

# append maturation score to imp (numerical)
mat  <- (as.integer(temp$gen) + as.integer(temp$phb)
+ as.integer(cut(temp$tv,breaks=c(0,3,6,10,15,20,25))))
imp2 <- cbind.mids(imp, as.data.frame(mat))

# append maturation score to imp (factor)
# known issue: new column name is 'y', not 'mat'
mat  <- as.factor(mat)
imp2 <- cbind.mids(imp, mat)

# append data frame with two columns to imp
temp2 <- data.frame(bmi=boys$bmi,mat=as.factor(mat))
imp2  <- cbind.mids(imp, temp2)

# combine two mids objects
impa <- mice(temp, maxit=1, m=2)
impb <- mice(temp2, maxit=2, m=2)

# first a then b
impab <- cbind.mids(impa, impb)

# first b then a
impba <- cbind.mids(impb, impa)
}
\author{
  Karin Groothuis-Oudshoorn, Stef van Buuren, 2009
}
\seealso{
  \code{\link{rbind.mids}}, \code{\link{ibind}},
  \code{\link[=mids-class]{mids}}
}
\keyword{manip}

