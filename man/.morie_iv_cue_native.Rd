% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_iv_cue_native}
\alias{.morie_iv_cue_native}
\title{Internal helper: continuously-updated GMM (CUE)}
\usage{
.morie_iv_cue_native(y, X, Z, max_iter = 200L, tol = 1e-10)
}
\description{
Minimizes n * gbar(beta)\' S(beta)^{-1} gbar(beta) over beta with stats::optim (BFGS), started at the two-step estimate.
}
\value{
A list with \code{beta}, \code{se}, \code{vcov}, \code{residuals}, \code{J}, \code{converged}, \code{n}.
}
\keyword{internal}
