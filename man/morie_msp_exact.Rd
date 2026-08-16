% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_msp_exact}
\alias{morie_msp_exact}
\title{morie_msp_exact}
\usage{
morie_msp_exact(query, subject, match = 5, mismatch = -4, matrix = NULL, alphabet = "ACGT")
}
\arguments{
\item{query}{See Usage.}
\item{subject}{See Usage.}
\item{match}{Defaults to \code{5}.}
\item{mismatch}{Defaults to \code{-4}.}
\item{matrix}{Defaults to \code{NULL}.}
\item{alphabet}{Defaults to \code{"ACGT"}.}
}
\description{
Part of the blstn_native implementation; see the file header for the source it follows.
}
\value{
A list with \code{score}, \code{qstart}, \code{sstart}, \code{length}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
morie_msp_exact(V, V)
}
