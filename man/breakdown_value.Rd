% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{breakdown_value}
\alias{breakdown_value}
\title{breakdown_value}
\usage{
breakdown_value(beta, n_pre, n_post, family = "SD", l_vec = NULL, sign = "positive", M_max = 10, tol = 1e-09)
}
\arguments{
\item{beta}{Passed to \code{identified_set}.}
\item{n_pre}{Passed to \code{identified_set}.}
\item{n_post}{Passed to \code{identified_set}.}
\item{family}{Carried through into a list the body builds. Defaults to \code{"SD"}.}
\item{l_vec}{Passed to \code{identified_set}.}
\item{sign}{One of \code{"negative"}, \code{"positive"}. Defaults to \code{"positive"}.}
\item{M_max}{Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{10}.}
\item{tol}{Passed to \code{>}. Defaults to \code{1e-09}.}
}
\description{
A step of the snmtst_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A list with \code{breakdown}, \code{family}, \code{sign}, \code{status}.
}
\keyword{internal}
\examples{
beta <- c(-0.05, 0.02, 0.5, 0.55, 0.6)
r <- breakdown_value(beta, n_pre = 2, n_post = 3)
str(r, max.level = 1)
}
