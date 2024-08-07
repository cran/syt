% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/enumeration.R
\name{firstsyt}
\alias{firstsyt}
\title{First tableau of a given shape}
\usage{
firstsyt(lambda)
}
\arguments{
\item{lambda}{the shape, an integer partition}
}
\value{
A standard Young tableau.
}
\description{
Returns the "first" standard Young tableau of a given shape.
}
\examples{
firstsyt(c(4, 2, 1))
}
