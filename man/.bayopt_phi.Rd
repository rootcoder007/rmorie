% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.bayopt_phi}
\alias{.bayopt_phi}
\title{.bayopt_phi}
\usage{
.bayopt_phi(z)
}
\arguments{
\item{z}{Numeric; combined arithmetically in the body.}
}
\description{
A step of the bayopt_native implementation. Called by \code{acquisition_gradient}, \code{expected_improvement}. See the file header for the source the module follows.
}
\value{
A numeric value.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.bayopt_phi(V)
}
