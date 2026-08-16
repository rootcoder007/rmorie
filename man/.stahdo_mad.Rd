% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.stahdo_mad}
\alias{.stahdo_mad}
\title{.stahdo_mad}
\usage{
.stahdo_mad(v, consistent = TRUE)
}
\arguments{
\item{v}{Numeric; combined arithmetically in the body.}
\item{consistent}{A flag; the body branches on it. Defaults to \code{TRUE}.}
}
\description{
A step of the stahdo_native implementation. Called by \code{.stahdo_outlyingness}. See the file header for the source the module follows.
}
\value{
One of two values, depending on the branch taken.
}
\references{
----------
Maronna, R. A. & Yohai, V. J. (1995) "The behavior of the
Stahel-Donoho robust multivariate estimator", *Journal of the
American Statistical Association* 90(429), 330-341,
doi:10.1080/01621459.1995.10476517.
}
\keyword{internal}
