% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.s03swish}
\alias{.s03swish}
\title{Swish_beta(x) = x sigma(beta x) (Ramachandran et al. 2017)}
\usage{
.s03swish(z, beta = 1)
}
\arguments{
\item{z}{Numeric; combined arithmetically in the body.}
\item{beta}{Numeric; combined arithmetically in the body. Defaults to \code{1}.}
}
\description{
A step of the helpers_s03 implementation. Called by \code{Llamablock}, \code{Mbconv}, \code{Swiglu}. See the file header for the source the module follows.
}
\value{
A numeric value.
}
\keyword{internal}
