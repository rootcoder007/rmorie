% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{mask_units}
\alias{mask_units}
\title{mask_units}
\usage{
mask_units(n_units, rate = 0.3, seed = 0, block = 1) 
}
\arguments{
\item{n_units}{Coerced to integer by the body, with \code{as.integer}.}
\item{rate}{Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{0.3}.}
\item{seed}{Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{0}.}
\item{block}{Coerced to integer by the body, with \code{as.integer}. Defaults to \code{1}.}
}
\description{
A step of the ocrwit_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A list with \code{masked}, \code{kept}, \code{rate}, \code{block}, \code{note}.
}
\keyword{internal}
\examples{
mask_units(n_units = 5L)
}
