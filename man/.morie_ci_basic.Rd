% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_ci_basic}
\alias{.morie_ci_basic}
\title{.morie_ci_basic}
\usage{
.morie_ci_basic(t0, t, conf)
}
\arguments{
\item{t0}{Numeric; combined arithmetically in the body.}
\item{t}{Passed to \code{.morie_norm_inter}.}
\item{conf}{Numeric; combined arithmetically in the body.}
}
\description{
A step of the boot_native implementation. Called by \code{morie_boot_ci}. See the file header for the source the module follows.
}
\value{
A numeric value.
}
\keyword{internal}
\examples{
.morie_ci_basic(t0 = c(1, 2, 3, 4, 5, 6, 7, 8), t = c(1, 2, 3, 4, 5, 6, 7, 8), conf = 0.5)
}
