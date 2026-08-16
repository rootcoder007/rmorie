% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_match_whiten}
\alias{.morie_match_whiten}
\title{Internal helper: control-pool whitening transform}
\usage{
.morie_match_whiten(X, X_control)
}
\description{
Internal helper: control-pool whitening transform
}
\value{
A matrix, from \code{t}.
}
\keyword{internal}
\examples{
M <- matrix(c(1, 2, 3, 4, 5, 6), nrow = 2)
.morie_match_whiten(M, M)
}
