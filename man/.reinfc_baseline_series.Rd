% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.reinfc_baseline_series}
\alias{.reinfc_baseline_series}
\title{.reinfc_baseline_series}
\usage{
.reinfc_baseline_series(rewards, baseline, gamma)
}
\arguments{
\item{rewards}{A vector; its length is taken and its elements indexed.}
\item{baseline}{One of \code{"comparison"}, \code{"none"}.}
\item{gamma}{Numeric; combined arithmetically in the body.}
}
\description{
A step of the reinfc_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
The value of \code{out}, as built in the body.
}
\keyword{internal}
\examples{
.reinfc_baseline_series(rewards = c(1, 2, 3, 4, 5, 6, 7, 8), baseline = 5L, gamma = 0.5)
}
