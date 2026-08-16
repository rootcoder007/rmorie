% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_tps_apply_nrows}
\alias{.morie_tps_apply_nrows}
\title{Internal helper: Morie Tps Apply Nrows}
\usage{
.morie_tps_apply_nrows(df, nrows)
}
\description{
Internal helper: Morie Tps Apply Nrows
}
\value{
The value of \code{[}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
M <- matrix(c(1, 2, 3, 4, 5, 6), nrow = 2)
.morie_tps_apply_nrows(M, V)
}
