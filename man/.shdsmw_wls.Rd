% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.shdsmw_wls}
\alias{.shdsmw_wls}
\title{.shdsmw_wls}
\usage{
.shdsmw_wls(X, y, w)
}
\arguments{
\item{X}{A matrix; passed to \code{nrow}.}
\item{y}{A matrix; passed to \code{crossprod}.}
\item{w}{Numeric; combined arithmetically in the body.}
}
\description{
A step of the shdsmw_native implementation. Called by \code{shrinkage_msm}. See the file header for the source the module follows.
}
\value{
A list with \code{coef}, \code{se}.
}
\keyword{internal}
\examples{
.shdsmw_wls(X = c(1, 2, 3, 4, 5, 6, 7, 8), y = c(1, 2, 3, 4, 5, 6, 7, 8), w = c(1, 2, 3, 4, 5, 6, 7, 8))
}
