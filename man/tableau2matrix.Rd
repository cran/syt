% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/syt2matrix.R
\name{tableau2matrix}
\alias{tableau2matrix}
\title{Tableau as sparse matrix}
\usage{
tableau2matrix(tableau)
}
\arguments{
\item{tableau}{a tableau with numeric or logical entries}
}
\value{
A sparse matrix.
}
\description{
Representation of a tableau as a sparse matrix; only for a 
  tableau with numeric or logical entries.
}
\examples{
syt <- list(c(1, 2, 6), c(3, 5), 4)
tableau2matrix(syt)
}
