% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.bootstrap_ci}
\alias{.bootstrap_ci}
\title{Internal helper: Bootstrap Ci}
\usage{
.bootstrap_ci(func, args, n_boot = 2000L, confidence = 0.95, seed = 42L)
}
\description{
Internal helper: Bootstrap Ci
}
\value{
A list with \code{se}, \code{ci_lo}, \code{ci_hi}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.bootstrap_ci(V, V)
}
