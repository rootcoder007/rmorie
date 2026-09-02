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
\item{base}{Passed to \code{mint_reconcile}.}
\item{S}{Passed to \code{mint_reconcile}.}
\item{method}{Passed to \code{mint_reconcile}. Defaults to \code{"shrink"}.}
\item{residuals}{Passed to \code{mint_reconcile}.}
\item{W}{Passed to \code{mint_reconcile}.}
\item{ridge}{Passed to \code{mint_reconcile}. Defaults to \code{1e-10}.}
}
\description{
A step of the hierF_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
The value of \code{mint_reconcile}.
}
\keyword{internal}
\examples{
set.seed(1)
S <- summing_matrix(list(c(0L, 1L)), 2L)
res <- matrix(rnorm(30), 10, 3)
r <- morie_hierF(c(10.5, 4.9, 5.4), S, residuals = res)
str(r, max.level = 1)
}
