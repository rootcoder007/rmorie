% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.stahdo_null_vector}
\alias{.stahdo_null_vector}
\title{.stahdo_null_vector}
\usage{
.stahdo_null_vector(rows, p)
}
\arguments{
\item{rows}{A matrix; passed to \code{nrow}.}
\item{p}{Accepted by the signature and not used anywhere in the body.}
}
\description{
A step of the stahdo_native implementation. Called by \code{.stahdo_subsample_dirs}. See the file header for the source the module follows.
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
