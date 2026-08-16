% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.ch_ols_se}
\alias{.ch_ols_se}
\title{OLS coefficient j (1-based), its standard error and the residual df}
\usage{
.ch_ols_se(y, X, j)
}
\arguments{
\item{y}{A vector; its length is taken.}
\item{X}{Passed to \code{.s03mat}.}
\item{j}{See Usage.}
}
\description{
A step of the chzlt implementation. Called by \code{Chzlt}. See the file header for the source the module follows.
}
\value{
A list with \code{beta}, \code{se}, \code{df}.
}
\keyword{internal}
\examples{
.ch_ols_se(y = c(1, 2, 3, 4, 5, 6, 7, 8), X = c(1, 2, 3, 4, 5, 6, 7, 8), j = c(1, 2, 3, 4, 5, 6, 7, 8))
}
