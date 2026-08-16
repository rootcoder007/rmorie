% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_spx_dft}
\alias{.morie_spx_dft}
\title{X_k = sum_u x_u exp(-i w_k u), u and k running from 0}
\usage{
.morie_spx_dft(x)
}
\arguments{
\item{x}{A vector; its length is taken.}
}
\description{
A step of the sp_fill implementation. Called by \code{CrossSpec}, \code{MsCoh}, \code{SpecAnom}. See the file header for the source the module follows.
}
\value{
A list with \code{re}, \code{im}.
}
\keyword{internal}
