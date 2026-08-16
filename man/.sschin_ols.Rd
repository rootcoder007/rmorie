% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.sschin_ols}
\alias{.sschin_ols}
\title{Least squares with a ridge scaled to the design, plus sigma}
\usage{
.sschin_ols(X, y, ridge_rel = 1e-08)
}
\arguments{
\item{X}{See Usage.}
\item{y}{See Usage.}
\item{ridge_rel}{Defaults to \code{1e-08}.}
}
\description{
Part of the sschin_native implementation; see the file header for the source it follows.
}
\value{
A list with \code{beta}, \code{sigma}.
}
\keyword{internal}
