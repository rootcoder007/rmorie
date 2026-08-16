% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.caus_intercept}
\alias{.caus_intercept}
\title{.caus_intercept}
\usage{
.caus_intercept(X)
}
\arguments{
\item{X}{A matrix; passed to \code{as.matrix}.}
}
\description{
A step of the caus_native implementation. Called by \code{.caus_first_stage_f}, \code{morie_caus_iv_2sls}, \code{morie_caus_iv_liml}. See the file header for the source the module follows.
}
\value{
The value of \code{cbind}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.caus_intercept(V)
}
