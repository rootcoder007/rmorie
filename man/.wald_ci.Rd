% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.wald_ci}
\alias{.wald_ci}
\title{Internal helper: Wald Ci}
\usage{
.wald_ci(estimate, se)
}
\description{
Internal helper: Wald Ci
}
\value{
A vector.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.wald_ci(V, V)
}
