% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.stahdo_chi2_median}
\alias{.stahdo_chi2_median}
\title{.stahdo_chi2_median}
\usage{
.stahdo_chi2_median(p)
}
\arguments{
\item{p}{Numeric; combined arithmetically in the body.}
}
\description{
A step of the stahdo_native implementation. Called by \code{.stahdo_stahel_donoho}. See the file header for the source the module follows.
}
\value{
A numeric value.
}
\references{
----------
Maronna, R. A. & Yohai, V. J. (1995) "The behavior of the
Stahel-Donoho robust multivariate estimator", *Journal of the
American Statistical Association* 90(429), 330-341,
doi:10.1080/01621459.1995.10476517.
}
\keyword{internal}
\examples{
.stahdo_chi2_median(p = 0.5)
}
