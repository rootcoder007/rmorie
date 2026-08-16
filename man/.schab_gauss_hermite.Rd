% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.schab_gauss_hermite}
\alias{.schab_gauss_hermite}
\title{Golub-Welsch: nodes are the eigenvalues of the symmetric tridiagonal}
\usage{
.schab_gauss_hermite(n)
}
\arguments{
\item{n}{A count; the body uses it as \code{matrix(...)}.}
}
\description{
Jacobi matrix (zero diagonal, sqrt(k) off-diagonal for the probabilists\\' Hermite system); weights are the squared first eigenvector components. Written out rather than taken from a package so this arm runs the same arithmetic as the Python one -- both need only a symmetric eigensolver. The weights sum to 1, so sum(w * g(x)) is E[g(X)] for X ~ N(0, 1).
}
\value{
A list with \code{nodes}, \code{weights}.
}
\keyword{internal}
