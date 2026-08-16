% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{concentrated_loglik}
\alias{concentrated_loglik}
\title{concentrated_loglik}
\usage{
concentrated_loglik(y, resid, omega)
}
\arguments{
\item{y}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{resid}{A vector; its length is taken.}
\item{omega}{Numeric; combined arithmetically in the body.}
}
\description{
A step of the bats_native implementation. Called by \code{.fit_spec}. See the file header for the source the module follows.
}
\value{
The value of \code{out}, as built in the body.
}
\keyword{internal}
\examples{
concentrated_loglik(y = c(1, 2, 3, 4, 5, 6, 7, 8), resid = c(1, 2, 3, 4, 5, 6, 7, 8), omega = 5L)
}
