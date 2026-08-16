% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.ecfp_conninv}
\alias{.ecfp_conninv}
\title{.ecfp_conninv}
\usage{
.ecfp_conninv(B, atomnum, numhs, charge, inring, isodelta)
}
\arguments{
\item{B}{A list; the body reads \code{$a}, \code{$i}, \code{$j} from it.}
\item{atomnum}{A vector; indexed elementwise.}
\item{numhs}{A vector; indexed elementwise.}
\item{charge}{A vector; indexed elementwise.}
\item{inring}{A vector; indexed elementwise.}
\item{isodelta}{A vector; indexed elementwise.}
}
\description{
A step of the ecfp4 implementation. Called by \code{Ecfp4}, \code{Ecfp6}. See the file header for the source the module follows.
}
\value{
The value of \code{inv}, as built in the body.
}
\keyword{internal}
