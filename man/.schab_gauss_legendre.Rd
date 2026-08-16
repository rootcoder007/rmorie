% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.schab_gauss_legendre}
\alias{.schab_gauss_legendre}
\title{Golub and Welsch (1969), Math. Comp. 23(106):221-230 -- NOT a}
\usage{
.schab_gauss_legendre(n)
}
\arguments{
\item{n}{See Usage.}
}
\description{
Schabenberger & Gotway result. Nodes are the eigenvalues of the Jacobi matrix; weights are mu_0 times the squared first eigenvector components, with mu_0 the ZEROTH MOMENT of the weight function -- 2 for Legendre on [-1, 1]. The Hermite rule elsewhere carries no such factor because there the Gaussian weight integrates to 1.
}
\value{
A list with \code{nodes}, \code{weights}.
}
\keyword{internal}
