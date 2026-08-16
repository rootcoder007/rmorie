% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_dp_gaussian_sigma}
\alias{.morie_dp_gaussian_sigma}
\title{.morie_dp_gaussian_sigma}
\usage{
.morie_dp_gaussian_sigma(sensitivity, epsilon, delta)
}
\arguments{
\item{sensitivity}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{epsilon}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{delta}{Numeric; combined arithmetically in the body.}
}
\description{
A step of the dp_native implementation. Called by \code{morie_dp_covariance}, \code{morie_dp_gaussian_mechanism}. See the file header for the source the module follows.
}
\value{
A numeric value.
}
\keyword{internal}
