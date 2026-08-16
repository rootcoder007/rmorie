% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{SpKappa}
\alias{SpKappa}
\title{Cohen\'s kappa (Cohen 1960) scored over NEIGHBOUR pairs rather than}
\usage{
SpKappa(x, y, w)
}
\arguments{
\item{x}{Passed to \code{.morie_spx_chkv}.}
\item{y}{Passed to \code{.morie_spx_chkv}.}
\item{w}{Passed to \code{.morie_spx_chkw}.}
}
\description{
same-site pairs; the pairing is Mantel\'s M2, eq (1.5), with U_ij = I{x_i = y_j}.  The kappa coefficient itself is NOT in the book.
}
\value{
A list with \code{kappa}, \code{p_observed}, \code{p_expected}, \code{categories}, \code{s0}, \code{compares_neighbours_not_same_site}, \code{n}, \code{method}.
}
\keyword{internal}
\examples{
set.seed(3)
W <- matrix(0, 8, 8)
W[abs(row(W) - col(W)) == 1] <- 1
SpKappa(rbinom(8, 1, 0.5), rbinom(8, 1, 0.5), W)
}
