% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.diff_ci}
\alias{.diff_ci}
\title{Internal helper: Diff Ci}
\usage{
.diff_ci(x, y, confidence = 0.95, equal_var = TRUE)
}
\description{
Internal helper: Diff Ci
}
\value{
A vector.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.diff_ci(V, V)
}
