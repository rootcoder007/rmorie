% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.mor_fp_defaults}
\alias{.mor_fp_defaults}
\title{.mor_fp_defaults}
\usage{
.mor_fp_defaults(a, numhs, charge, inring, isotope_delta)
}
\arguments{
\item{a}{A count; the body uses it as \code{rep(...)}.}
\item{numhs}{Passed to \code{col}.}
\item{charge}{Passed to \code{col}.}
\item{inring}{Passed to \code{col}.}
\item{isotope_delta}{Passed to \code{col}.}
}
\description{
A step of the ecfp4_native implementation. Called by \code{morie_ecfp4}, \code{morie_ecfp6}. See the file header for the source the module follows.
}
\value{
A list with \code{numhs}, \code{charge}, \code{inring}, \code{isotope_delta}.
}
\keyword{internal}
