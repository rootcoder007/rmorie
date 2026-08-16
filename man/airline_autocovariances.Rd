% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{airline_autocovariances}
\alias{airline_autocovariances}
\title{airline_autocovariances}
\usage{
airline_autocovariances(theta, Theta, sigma2 = 1)
}
\arguments{
\item{theta}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{Theta}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{sigma2}{Numeric; combined arithmetically in the body. Defaults to \code{1}.}
}
\description{
A step of the sarima_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A list with \code{gamma}, \code{rho}, \code{rho_1}, \code{rho_12}, \code{nonzero_lags}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
airline_autocovariances(V, V)
}
