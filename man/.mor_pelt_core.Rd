% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.mor_pelt_core}
\alias{.mor_pelt_core}
\title{.mor_pelt_core}
\usage{
.mor_pelt_core(x, cost, penalty, min_seglen)
}
\arguments{
\item{x}{A vector; its length is taken.}
\item{cost}{Passed to \code{.mor_cp_cost}.}
\item{penalty}{See Usage.}
\item{min_seglen}{Passed to \code{seq.int}.}
}
\description{
A step of the pelt_native implementation. Called by \code{morie_pelt}. See the file header for the source the module follows.
}
\value{
A list with \code{taus}, \code{objective}.
}
\keyword{internal}
