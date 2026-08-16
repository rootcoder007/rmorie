% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.stahdo_prep}
\alias{.stahdo_prep}
\title{.stahdo_prep}
\usage{
.stahdo_prep(X)
}
\arguments{
\item{X}{A matrix; passed to \code{as.matrix}.}
}
\description{
A step of the stahdo_native implementation. Called by \code{.stahdo_outlyingness}, \code{.stahdo_stahel_donoho}. See the file header for the source the module follows.
}
\value{
A list with \code{M}, \code{n}, \code{p}.
}
\references{
----------
Maronna, R. A. & Yohai, V. J. (1995) "The behavior of the
Stahel-Donoho robust multivariate estimator", *Journal of the
American Statistical Association* 90(429), 330-341,
doi:10.1080/01621459.1995.10476517.
}
\keyword{internal}
