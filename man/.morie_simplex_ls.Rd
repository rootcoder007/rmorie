% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_simplex_ls}
\alias{.morie_simplex_ls}
\title{Internal helper: simplex-constrained least squares}
\usage{
.morie_simplex_ls(A, b, zeta2 = 0, max_iter = 10000L, tol = 1e-12)
}
\description{
Minimizes ||b - A w||_2^2 (+ zeta^2 * n_reg * ||w||_2^2) subject to w >= 0, sum(w) = 1, by accelerated projected gradient (FISTA) with exact simplex projection. Deterministic.
}
\value{
The value of \code{w}, as built in the body.
}
\keyword{internal}
