% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_scm_weights}
\alias{.morie_scm_weights}
\title{Internal helper: synthetic-control weights for one treated unit}
\usage{
.morie_scm_weights(X1, X0, Z1, Z0, optimize_v = TRUE)
}
\description{
`X1` (k-vector) treated predictors, `X0` (k x J) donor predictors, `Z1` (T0-vector) treated pre-period outcomes, `Z0` (T0 x J) donor pre-period outcomes. Nested optimization: the outer loop picks the diagonal V (softmax-parameterized, via optim/Nelder-Mead) that minimizes pre-period outcome MSPE of the inner simplex-LS solution.
}
\value{
The value of \code{best}, as built in the body.
}
\keyword{internal}
