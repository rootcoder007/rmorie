% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.caus_k_class}
\alias{.caus_k_class}
\title{The k-class family. k = 0 is least squares, k = 1 is two-stage}
\usage{
.caus_k_class(y, X, Z, k)
}
\arguments{
\item{y}{A matrix; passed to \code{crossprod}.}
\item{X}{A matrix; passed to \code{t}.}
\item{Z}{Passed to \code{.caus_annihilate}.}
\item{k}{Numeric; combined arithmetically in the body.}
}
\description{
least squares, and k = the Anderson-Rubin ratio is LIML. Keeping them one function is what stops them drifting apart.
}
\value{
A vector, from \code{as.numeric}.
}
\keyword{internal}
