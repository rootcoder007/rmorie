% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{ibp_log_probability}
\alias{ibp_log_probability}
\title{ibp_log_probability}
\usage{
ibp_log_probability(Z, alpha)
}
\arguments{
\item{Z}{A matrix; passed to \code{as.matrix}.}
\item{alpha}{See Usage.}
}
\description{
A step of the ibpfa_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
The value of \code{lp}, as built in the body.
}
\keyword{internal}
\examples{
ibp_log_probability(Z = c(1, 2, 3, 4, 5, 6, 7, 8), alpha = 0.5)
}
