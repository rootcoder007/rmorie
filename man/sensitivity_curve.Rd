% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{sensitivity_curve}
\alias{sensitivity_curve}
\title{sensitivity_curve}
\usage{
sensitivity_curve(beta, n_pre, n_post, Ms, family = "SD", l_vec = NULL)
}
\arguments{
\item{beta}{Passed to \code{identified_set}.}
\item{n_pre}{Passed to \code{identified_set}.}
\item{n_post}{Passed to \code{identified_set}.}
\item{Ms}{Iterated over elementwise, with \code{lapply}.}
\item{family}{Carried through into a list the body builds. Defaults to \code{"SD"}.}
\item{l_vec}{Passed to \code{identified_set}.}
}
\description{
A step of the snmtst_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A list with \code{curve}, \code{family}, \code{M}, \code{width}.
}
\keyword{internal}
\examples{
beta <- c(-0.05, 0.02, 0.5, 0.55, 0.6)
r <- sensitivity_curve(beta, n_pre = 2, n_post = 3,
                       Ms = c(0, 0.05, 0.1))
str(r, max.level = 1)
}
