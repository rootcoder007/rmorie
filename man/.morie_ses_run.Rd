% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_ses_run}
\alias{.morie_ses_run}
\title{.morie_ses_run}
\usage{
.morie_ses_run(a, series)
}
\arguments{
\item{a}{Numeric; combined arithmetically in the body.}
\item{series}{A vector; its length is taken and its elements indexed.}
}
\description{
A step of the forecast_native implementation. Called by \code{morie_joseph_simple_exponential_smoothing}, \code{morie_theta_method}. See the file header for the source the module follows.
}
\value{
A list with \code{fitted}, \code{level}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.morie_ses_run(V, V)
}
