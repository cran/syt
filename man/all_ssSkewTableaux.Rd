% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/skewTableaux.R
\name{all_ssSkewTableaux}
\alias{all_ssSkewTableaux}
\title{Semistandard skew tableaux}
\usage{
all_ssSkewTableaux(lambda, mu, n)
}
\arguments{
\item{lambda, mu}{integer partitions defining the skew partition: 
\code{lambda} is the outer partition and \code{mu} is the inner partition 
(so \code{mu} must be a subpartition of \code{lambda})}

\item{n}{a positive integer, the maximum entry of the skew tableaux}
}
\value{
The list of all semistandard skew tableaux whose shape is the skew 
  partition defined by \code{lambda} and \code{mu} and with maximum entry 
  \code{n}.
}
\description{
Enumeration of all semistandard skew tableaux with given shape 
  and given maximum entry.
}
\examples{
ssstx <- all_ssSkewTableaux(c(4, 3, 1), c(2, 2), 2)
lapply(ssstx, prettySkewTableau)
}
\seealso{
\code{\link{all_ssytx}},
  \code{\link{skewTableauxWithGivenShapeAndWeight}}.
}
