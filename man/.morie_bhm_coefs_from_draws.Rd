% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_bhm_coefs_from_draws}
\alias{.morie_bhm_coefs_from_draws}
\title{Internal helper: Morie Bhm Coefs From Draws}
\usage{
.morie_bhm_coefs_from_draws(bmat, terms)
}
\description{
Internal helper: Morie Bhm Coefs From Draws
}
\value{
A data frame.
}
\keyword{internal}
\examples{
M <- matrix(c(1, 2, 3, 4, 5, 6), nrow = 2)
S <- c("a", "b", "c")
.morie_bhm_coefs_from_draws(M, S)
}
