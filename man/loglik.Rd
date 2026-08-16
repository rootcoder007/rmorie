% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{loglik}
\alias{loglik}
\title{loglik}
\usage{
loglik(w, ar = numeric(0), ma = numeric(0))
}
\arguments{
\item{w}{See Usage.}
\item{ar}{Defaults to \code{numeric(0)}.}
\item{ma}{Defaults to \code{numeric(0)}.}
}
\description{
Part of the sarima_native implementation; see the file header for the source it follows.
}
\value{
A list with \code{loglik}, \code{sigma2}, \code{n}, \code{exact_ssq}, \code{sum_log_f}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
loglik(V)
}
