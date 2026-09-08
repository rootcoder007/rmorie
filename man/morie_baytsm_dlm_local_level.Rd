% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_baytsm_dlm_local_level}
\alias{morie_baytsm_dlm_local_level}
\title{morie_baytsm_dlm_local_level}
\usage{
morie_baytsm_dlm_local_level(y, V = 1, W = 0.1, m0 = 0, C0 = 1e+06) 
}
\arguments{
\item{y}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{V}{Numeric; combined arithmetically in the body. Defaults to \code{1}.}
\item{W}{Numeric; combined arithmetically in the body. Defaults to \code{0.1}.}
\item{m0}{Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{0}.}
\item{C0}{Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{1e+06}.}
}
\description{
A step of the baytsm_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A list with \code{estimate}, \code{smoothed}, \code{smoothed_var}, \code{filtered}, \code{filtered_var}, \code{forecast}, \code{forecast_var}, \code{adaptive_coefficient}, \code{forecast_error}, \code{loglik}, \code{signal_to_noise}, \code{n}, \code{V}, \code{W}, \code{method}, \code{note}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
morie_baytsm_dlm_local_level(V)
}
