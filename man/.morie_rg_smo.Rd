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
\item{maxiter}{See Usage.}
\item{tol}{Numeric; combined arithmetically in the body.}
}
\description{
equality constraint sum a_i y_i = 0 that single-coordinate updates cannot
}
\value{
A list with \code{a}, \code{b}, \code{it}.
}
\keyword{internal}
