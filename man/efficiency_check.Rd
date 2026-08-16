% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{efficiency_check}
\alias{efficiency_check}
\title{efficiency_check}
\usage{
efficiency_check(err_Q, err_g, delta, n, donsker = TRUE)
}
\arguments{
\item{err_Q}{See Usage.}
\item{err_g}{See Usage.}
\item{delta}{See Usage.}
\item{n}{See Usage.}
\item{donsker}{Defaults to \code{TRUE}.}
}
\description{
Part of the tlhaltm_native implementation; see the file header for the source it follows.
}
\value{
A list with \code{estimate}, \code{remainder_bound}, \code{root_n}, \code{remainder_negligible}, \code{donsker_satisfied}, \code{efficient}, \code{positivity_delta}, \code{method}, \code{note}.
}
\keyword{internal}
\examples{
efficiency_check(err_Q = 5L, err_g = 5L, delta = 0.5, n = 5L)
}
