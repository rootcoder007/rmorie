% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_logit_fit}
\alias{.morie_logit_fit}
\title{Newton-Raphson logistic regression; returns fitted probabilities}
\usage{
.morie_logit_fit(X, y, max_iter = 100L, tol = 1e-09)
}
\arguments{
\item{X}{See Usage.}
\item{y}{Numeric; combined arithmetically in the body.}
\item{max_iter}{A count; the body uses it as \code{seq_len(...)}. Defaults to \code{100L}.}
\item{tol}{Defaults to \code{1e-09}.}
}
\description{
A step of the causal_shared_native implementation. Called by \code{morie_dr_learner}, \code{morie_regime_value}, \code{morie_tmle_ate} and 3 others in the module. See the file header for the source the module follows.
}
\value{
The value of \code{as.vector}.
}
\keyword{internal}
