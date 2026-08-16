% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_tlhaltm}
\alias{morie_tlhaltm}
\title{morie_tlhaltm}
\usage{
morie_tlhaltm(rate_Q = NULL, rate_g = NULL, n = NULL, err_Q = NULL, err_g = NULL, delta = NULL, donsker = TRUE, mode = c("rate", "remainder", "efficiency", "split"))
}
\arguments{
\item{rate_Q}{Passed to \code{rate_condition}.}
\item{rate_g}{Passed to \code{rate_condition}.}
\item{n}{Passed to \code{rate_condition}.}
\item{err_Q}{Passed to \code{remainder_bound}.}
\item{err_g}{Passed to \code{remainder_bound}.}
\item{delta}{Passed to \code{remainder_bound}.}
\item{donsker}{Passed to \code{efficiency_check}. Defaults to \code{TRUE}.}
\item{mode}{One of \code{"rate"}, \code{"remainder"}, \code{"split"}.}
}
\description{
A step of the tlhaltm_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
The value of \code{efficiency_check}.
}
\keyword{internal}
