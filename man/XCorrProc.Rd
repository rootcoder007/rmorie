% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{XCorrProc}
\alias{XCorrProc}
\title{The ensemble expectation is estimated by a TIME average, which is}
\usage{
XCorrProc(x, y, lags = NULL, remove_mean = TRUE)
}
\arguments{
\item{x}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{y}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{lags}{Optional; may be \code{NULL}. Coerced to integer by the body, with \code{as.integer}.}
\item{remove_mean}{A flag; the body branches on it. Defaults to \code{TRUE}.}
}
\description{
only legitimate under joint stationarity and ergodicity.
}
\value{
A list with \code{ccf}, \code{lags}, \code{means}, \code{mean_product}, \code{mean_removed}, \code{is_cross_covariance_when_mean_removed}, \code{expectation_estimated_by_time_average}, \code{requires_joint_stationarity_and_ergodicity}, \code{method}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
XCorrProc(V, V)
}
