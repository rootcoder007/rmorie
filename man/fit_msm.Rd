% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{fit_msm}
\alias{fit_msm}
\title{fit_msm}
\usage{
fit_msm(outcome, cumulative, weights = NULL, states = .STATES) 
}
\arguments{
\item{outcome}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{cumulative}{A matrix; passed to \code{as.matrix}.}
\item{weights}{Optional; may be \code{NULL}. Coerced to numeric by the body, with \code{as.numeric}.}
\item{states}{Coerced to character by the body, with \code{as.character}. Defaults to \code{.STATES}.}
}
\description{
A step of the bhltmsm_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A list with \code{estimate}, \code{intercept}, \code{coefficients}, \code{se}, \code{per_episode}, \code{weighted}, \code{effective_n}, \code{method}, \code{note}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
fit_msm(V, V)
}
