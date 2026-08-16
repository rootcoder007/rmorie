% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{NmfMu}
\alias{NmfMu}
\title{Section 9.7.3, eqs (9.46), (9.49)-(9.50) and (9.53)-(9.55).  PCA and}
\usage{
NmfMu(V, r, maxiter = 200, tol = 1e-10, cost = "ls", seed = 1)
}
\arguments{
\item{V}{Passed to \code{.morie_bx_mat}.}
\item{r}{Passed to \code{.morie_bx_nmfmu}.}
\item{maxiter}{Defaults to \code{200}.}
\item{tol}{Defaults to \code{1e-10}.}
\item{cost}{One of \code{"kld"}, \code{"ls"}. Defaults to \code{"ls"}.}
\item{seed}{Passed to \code{.morie_bx_nmfmu}. Defaults to \code{1}.}
}
\description{
ICA are free to use negative coefficients so their components cancel; nonnegativity makes the columns of W basis vectors and the rows of H the activations that switch them on.  The book warns the divergence form is undefined where V or WH has a zero.
}
\value{
A list with \code{W}, \code{H}, \code{submatrices}, \code{error}, \code{iterations}, \code{cost}, \code{method}.
}
\keyword{internal}
