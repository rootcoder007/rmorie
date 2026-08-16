% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.lcwphr_logit_irls}
\alias{.lcwphr_logit_irls}
\title{Logistic regression by IRLS; the ridge is scaled to the design}
\usage{
.lcwphr_logit_irls(X, y, max_iter = 100L, tol = 1e-11, ridge_rel = 1e-08)
}
\arguments{
\item{X}{See Usage.}
\item{y}{See Usage.}
\item{max_iter}{Defaults to \code{100L}.}
\item{tol}{Defaults to \code{1e-11}.}
\item{ridge_rel}{Defaults to \code{1e-08}.}
}
\description{
Part of the lcwphr_native implementation; see the file header for the source it follows.
}
\value{
A list with \code{beta}, \code{fitted}.
}
\keyword{internal}
