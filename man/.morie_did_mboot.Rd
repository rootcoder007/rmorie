% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_did_mboot}
\alias{.morie_did_mboot}
\title{Internal helper: Mammen multiplier bootstrap over an IF matrix}
\usage{
.morie_did_mboot(IF_mat, biters = 999L, seed = NULL, cluster = NULL)
}
\description{
Structure of `did::mboot`: draw Mammen(1993) two-point weights per unit, perturb the influence functions, and report the robust (IQR-based) standard deviation of the bootstrap draws.
}
\value{
A list with \code{se}, \code{boot}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.morie_did_mboot(V)
}
