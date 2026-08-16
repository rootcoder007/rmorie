% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_hierF}
\alias{morie_hierF}
\title{morie_hierF}
\usage{
morie_hierF(base, S, method = "shrink", residuals = NULL, W = NULL, ridge = 1e-10)
}
\arguments{
\item{base}{See Usage.}
\item{S}{See Usage.}
\item{method}{Defaults to \code{"shrink"}.}
\item{residuals}{Defaults to \code{NULL}.}
\item{W}{Defaults to \code{NULL}.}
\item{ridge}{Defaults to \code{1e-10}.}
}
\description{
A step of the hierF_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
The value of \code{mint_reconcile}.
}
\keyword{internal}
