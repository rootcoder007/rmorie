% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_mpfn_readout}
\alias{morie_mpfn_readout}
\title{Readout R}
\usage{
morie_mpfn_readout(H, how = "sum", H0 = NULL, i_fn = NULL, j_fn = NULL)
}
\arguments{
\item{H}{n x d matrix.}
\item{how}{"sum", "mean" or "gated".}
\item{H0,i_fn,j_fn}{Required for the gated readout.}
}
\description{
Readout R
}
\value{
A vector of length d.
}
\keyword{internal}
