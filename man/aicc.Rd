% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{aicc}
\alias{aicc}
\title{aicc}
\usage{
aicc(loglik, n_par, n)
}
\arguments{
\item{loglik}{Passed to \code{aic}.}
\item{n_par}{Coerced to integer by the body, with \code{as.integer}.}
\item{n}{Numeric; combined arithmetically in the body.}
}
\description{
A step of the sarimax_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A numeric value.
}
\keyword{internal}
\examples{
aicc(loglik = c(1, 2, 3, 4, 5, 6, 7, 8), n_par = 5L, n = 5L)
}
