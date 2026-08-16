% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{rate_condition}
\alias{rate_condition}
\title{rate_condition}
\usage{
rate_condition(rate_Q, rate_g, n)
}
\arguments{
\item{rate_Q}{See Usage.}
\item{rate_g}{See Usage.}
\item{n}{See Usage.}
}
\description{
A step of the tlhaltm_native implementation. Called by \code{morie_tlhaltm}. See the file header for the source the module follows.
}
\value{
A list with \code{sum}, \code{required}, \code{satisfied}, \code{product_order}, \code{root_n_order}, \code{note}.
}
\keyword{internal}
\examples{
rate_condition(rate_Q = 5L, rate_g = 5L, n = 5L)
}
