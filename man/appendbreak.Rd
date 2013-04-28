\name{appendbreak}
\alias{appendbreak}
\title{Appends specified break to the data}
\usage{
  appendbreak(data, brk, warp.model = warp.model,
    id = NULL, typ = "pred")
}
\arguments{
  \item{data}{A data frame in the long long format}

  \item{brk}{A vector of break ages}

  \item{warp.model}{A time warping model}

  \item{id}{The subject identifier}

  \item{typ}{Label to signal that this is a newly added
  observation}
}
\value{
  A long data frame with additional rows for the break ages
}
\description{
  A custom function to insert rows in long data with new
  pseudo-observations that are being done on the specified
  break ages. There should be a column called \code{first}
  in \code{data} with logical data that codes whether the
  current row is the first for subject \code{id}.
  Furthermore, the function assumes that columns
  \code{age}, \code{occ}, \code{hgt.z}, \code{wgt.z} and
  \code{bmi.z} are available. This function is used on the
  \code{tbc} data in FIMD chapter 9. Check that out to see
  it in action.
}

