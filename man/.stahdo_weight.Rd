% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.stahdo_weight}
\alias{.stahdo_weight}
\title{.stahdo_weight}
\usage{
.stahdo_weight(r, cutoff)
}
\arguments{
\item{r}{Numeric; combined arithmetically in the body.}
\item{cutoff}{Coerced to numeric by the body, with \code{as.numeric}.}
}
\description{
A step of the stahdo_native implementation. Called by \code{.stahdo_stahel_donoho}. See the file header for the source the module follows.
}
\value{
The value of \code{ifelse}.
}
\references{
----------
Maronna, R. A. & Yohai, V. J. (1995) "The behavior of the
Stahel-Donoho robust multivariate estimator", *Journal of the
American Statistical Association* 90(429), 330-341,
doi:10.1080/01621459.1995.10476517.
}
\keyword{internal}
