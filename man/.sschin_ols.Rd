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
\item{X}{A matrix; passed to \code{nrow}.}
\item{y}{A matrix; passed to \code{crossprod}.}
\item{ridge_rel}{Numeric; combined arithmetically in the body. Defaults to \code{1e-08}.}
}
\description{
A step of the sschin_native implementation. Called by \code{morie_sschin_chained_imputation}. See the file header for the source the module follows.
}
\value{
A list with \code{beta}, \code{sigma}.
}
\keyword{internal}
