% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{Ma8Sinc}
\alias{Ma8Sinc}
\title{Eq (3.122): the Dirichlet kernel, a real sinc-like envelope times a}
\usage{
Ma8Sinc(omega)
}
\arguments{
\item{omega}{See Usage.}
}
\description{
pure delay of 7/2 samples.  The book states it "is equivalent to that in Equation 3.111", and both are recomputed and compared.  The delay is not an integer -- the price of an even-length filter.
}
\value{
A list with \code{H}, \code{omega}, \code{direct_sum}, \code{max_difference}, \code{agrees_with_eq_3_111}, \code{group_delay}, \code{delay_is_not_an_integer}, \code{method}.
}
\keyword{internal}
