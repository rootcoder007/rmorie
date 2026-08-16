% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.remlfn_loglik}
\alias{.remlfn_loglik}
\title{.remlfn_loglik}
\usage{
.remlfn_loglik(gs, ns, s2a, s2e)
}
\arguments{
\item{gs}{A vector; its length is taken and its elements indexed.}
\item{ns}{A vector; indexed elementwise.}
\item{s2a}{Numeric; combined arithmetically in the body.}
\item{s2e}{Numeric; passed to \code{log}.}
}
\description{
A step of the remlfn_native implementation. Called by \code{morie_remlfn}. See the file header for the source the module follows.
}
\value{
A list with \code{loglik}, \code{mu}.
}
\keyword{internal}
\examples{
.remlfn_loglik(gs = c(1, 2, 3, 4, 5, 6, 7, 8), ns = c(1, 2, 3, 4, 5, 6, 7, 8), s2a = c(1, 2, 3, 4, 5, 6, 7, 8), s2e = c(1, 2, 3, 4, 5, 6, 7, 8))
}
