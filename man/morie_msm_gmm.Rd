% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_msm_gmm}
\alias{morie_msm_gmm}
\title{morie_msm_gmm}
\usage{
morie_msm_gmm(y, X, Z, weights = NULL) 
}
\description{
morie_msm_gmm
}
\arguments{
\item{y}{Argument \code{y}; see Usage.}
\item{X}{Argument \code{X}; see Usage.}
\item{Z}{Argument \code{Z}; see Usage.}
\item{weights}{Argument \code{weights}; see Usage.}
}
\value{
A list with \code{beta}, \code{residuals}, \code{moments}.
}
\keyword{internal}
\examples{
rmorie:::morie_msm_gmm(y = c(1, 2, 3, 4, 5, 6, 7, 8), X = c(1, 2, 3, 4, 5, 6, 7, 8),
  Z = c(1, 2, 3, 4, 5, 6, 7, 8))
}
