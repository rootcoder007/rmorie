% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.grclus_bisect}
\alias{.grclus_bisect}
\title{.grclus_bisect}
\usage{
.grclus_bisect(adj, vw, target, matching, initial, refinement, tolerance, coarsest, seed)
}
\arguments{
\item{adj}{Passed to \code{.grclus_balance_bisection}.}
\item{vw}{Numeric; passed to \code{sum}.}
\item{target}{Numeric; combined arithmetically in the body.}
\item{matching}{Passed to \code{.grclus_match_vertices}.}
\item{initial}{Compared against \code{"gggp"}.}
\item{refinement}{Compared against \code{"bkl"}.}
\item{tolerance}{Passed to \code{.grclus_kl}.}
\item{coarsest}{See Usage.}
\item{seed}{Numeric; combined arithmetically in the body.}
}
\description{
A step of the grclus_native implementation. Called by \code{morie_grclus}. See the file header for the source the module follows.
}
\value{
The value of \code{.grclus_balance_bisection}.
}
\keyword{internal}
