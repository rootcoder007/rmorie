% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{Acfseg}
\alias{Acfseg}
\title{Section 8.5.2, eqs. (8.27)-(8.29), after Michael and Houchin}
\usage{
Acfseg(test, reference, lags = NULL, thp = 1, thf = 1)
}
\arguments{
\item{test}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{reference}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{lags}{Optional; may be \code{NULL}. Coerced to integer by the body, with \code{as.integer}.}
\item{thp}{Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{1}.}
\item{thf}{Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{1}.}
}
\description{
A step of the rangayyan_adapt implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A list with \code{distance}, \code{power_distance}, \code{spectral_distance}, \code{lags}, \code{lags_auto}, \code{acf_test}, \code{acf_reference}, \code{power_test}, \code{power_reference}, \code{boundary}, \code{th_power}, \code{th_spectral}, \code{amplitude_invariant}, \code{method}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
Acfseg(V, V)
}
