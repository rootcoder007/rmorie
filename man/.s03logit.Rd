% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.s03logit}
\alias{.s03logit}
\title{Logistic regression by IRLS: Newton-Raphson on the log-likelihood,}
\usage{
.s03logit(X, y, iters = 60L, ridge = 1e-10, tol = 1e-13)
}
\arguments{
\item{X}{See Usage.}
\item{y}{See Usage.}
\item{iters}{Defaults to \code{60L}.}
\item{ridge}{Defaults to \code{1e-10}.}
\item{tol}{Defaults to \code{1e-13}.}
}
\description{
which for the canonical link is exactly IRLS, beta <- beta + (X\\\' W X)^-1 X\\\' (y - p), W = diag(p (1 - p)).
}
\value{
The value of \code{beta}, as built in the body.
}
\keyword{internal}
