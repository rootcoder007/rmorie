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
\item{y}{See Usage.}
\item{max_iter}{Defaults to \code{100L}.}
\item{tol}{Defaults to \code{1e-09}.}
}
\description{
Part of the causal_shared_native implementation; see the file header for the source it follows.
}
\value{
The value of \code{as.vector}.
}
\keyword{internal}
