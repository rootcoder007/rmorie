% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{fixed_length_ci}
\alias{fixed_length_ci}
\title{fixed_length_ci}
\usage{
fixed_length_ci(beta, sigma, n_pre, n_post, M = 0, family = "SD", l_vec = NULL, 
    level = 0.95) 
}
\arguments{
\item{beta}{Passed to \code{identified_set}.}
\item{sigma}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{n_pre}{Passed to \code{identified_set}.}
\item{n_post}{Passed to \code{identified_set}.}
\item{M}{Passed to \code{identified_set}. Defaults to \code{0}.}
\item{family}{Carried through into a list the body builds. Defaults to \code{"SD"}.}
\item{l_vec}{Passed to \code{identified_set}.}
\item{level}{Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{0.95}.}
}
\description{
A step of the snmtst_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A list with \code{estimate}, \code{lower}, \code{upper}, \code{identified_lower}, \code{identified_upper}, \code{M}, \code{family}, \code{level}, \code{conservative}, \code{method}.
}
\keyword{internal}
\examples{
beta <- c(-0.05, 0.02, 0.5, 0.55, 0.6)
r <- fixed_length_ci(beta, sigma = 0.1, n_pre = 2, n_post = 3, M = 0.1)
str(r, max.level = 1)
}
