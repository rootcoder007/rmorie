% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_rg_smo}
\alias{.morie_rg_smo}
\title{Shared SMO-style coordinate ascent on pairs, which respects the}
\usage{
.morie_rg_smo(K, ys, Cv, maxiter, tol)
}
\arguments{
\item{K}{A matrix; indexed by row and column.}
\item{ys}{A vector; its length is taken and its elements indexed.}
\item{Cv}{Numeric; passed to \code{min}.}
\item{maxiter}{Coerced to integer by the body, with \code{as.integer}.}
\item{tol}{Numeric; combined arithmetically in the body.}
}
\description{
equality constraint sum a_i y_i = 0 that single-coordinate updates cannot
}
\value{
A list with \code{a}, \code{b}, \code{it}.
}
\keyword{internal}
\examples{
XC <- matrix(c(1, 1, 1.4, 0.8, 0.7, 1.3, 1.1, 1.6, 3, 3, 3.3, 
    2.7, 2.6, 3.2, 3.1, 3.5), ncol = 2, byrow = TRUE)
YC <- c(0, 0, 0, 0, 1, 1, 1, 1)
FishLda(XC, YC)
.morie_rg_smo(K = XC, ys = XC, Cv = XC, maxiter = YC, tol = XC)
}
