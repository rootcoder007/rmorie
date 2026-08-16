% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{Penfreg}
\alias{Penfreg}
\title{Penalized functional regression fit (MVSML eq. 14.12)}
\usage{
Penfreg(y, X, P, lam, mu = NULL, tol = 1e-10)
}
\description{
With the spectral decomposition P = Gamma D Gamma\', X* = X Gamma and beta* = Gamma\' beta, (14.10) becomes SSE_lambda(beta*) = ||y - 1_n mu - X* beta*||^2 + lambda beta*\' D beta*, minimized at beta* = (X*\'X* + lambda D)^-1 X*\'(y - 1_n mu), with beta = Gamma beta*.  Zero eigenvalues of a rank-deficient P contribute nothing, the reduction the book notes.
}
\value{
A list with \code{beta}, \code{beta_star}, \code{Gamma}, \code{eigenvalues}, \code{X_star}, \code{mu}, \code{fitted}, \code{residuals}, \code{sse}, \code{penalty}, \code{objective}, \code{rank}.
}
\keyword{internal}
\examples{
Penfreg(y = c(1, 2, 3, 4, 5, 6, 7, 8), X = c(1, 2, 3, 4, 5, 6, 7, 8), P = 0.5, lam = c(1, 2, 3, 4, 5, 6, 7, 8))
}
