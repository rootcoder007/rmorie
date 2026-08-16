% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.mor_fp_invariants}
\alias{.mor_fp_invariants}
\title{.mor_fp_invariants}
\usage{
.mor_fp_invariants(a, bd, atomnum, numhs, charge, inring, isotope_delta)
}
\arguments{
\item{a}{A count; the body uses it as \code{seq_len(...)}.}
\item{bd}{A list; the body reads \code{$i}, \code{$j} from it.}
\item{atomnum}{A vector; indexed elementwise.}
\item{numhs}{A vector; indexed elementwise.}
\item{charge}{A vector; indexed elementwise.}
\item{inring}{A vector; indexed elementwise.}
\item{isotope_delta}{A vector; indexed elementwise.}
}
\description{
A step of the ecfp4_native implementation. Called by \code{morie_ecfp4}, \code{morie_ecfp6}. See the file header for the source the module follows.
}
\value{
The value of \code{inv}, as built in the body.
}
\keyword{internal}
