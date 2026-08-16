% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.gprkhspredict}
\alias{.gprkhspredict}
\title{.gprkhspredict}
\usage{
.gprkhspredict(K_new, beta, eta0 = 0)
}
\arguments{
\item{K_new}{Passed to \code{.gpmat}.}
\item{beta}{Passed to \code{.gpflat}.}
\item{eta0}{Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{0}.}
}
\description{
A step of the helpers_gp_core implementation. Called by \code{.gprkhsfitsq}, \code{Msm123}. See the file header for the source the module follows.
}
\value{
A numeric value.
}
\keyword{internal}
