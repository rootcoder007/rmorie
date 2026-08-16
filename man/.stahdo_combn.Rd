% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.stahdo_combn}
\alias{.stahdo_combn}
\title{.stahdo_combn}
\usage{
.stahdo_combn(n, p)
}
\arguments{
\item{n}{Numeric; combined arithmetically in the body.}
\item{p}{A count; the body uses it as \code{seq_len(...)}.}
}
\description{
A step of the stahdo_native implementation. Called by \code{.stahdo_subsample_dirs}. See the file header for the source the module follows.
}
\value{
The value of \code{result}, as built in the body.
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
.stahdo_combn(n = 5L, p = 0.5)
}
