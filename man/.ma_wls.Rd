% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.ma_wls}
\alias{.ma_wls}
\title{Weighted least squares. cov is (X\\' W X)^{-1}, the model-based}
\usage{
.ma_wls(X, y, w)
}
\arguments{
\item{X}{A matrix; passed to \code{ncol}.}
\item{y}{A matrix; passed to \code{crossprod}.}
\item{w}{Numeric; combined arithmetically in the body.}
}
\description{
covariance -- correct when the weights really are inverse variances, which is the whole premise of inverse-variance meta-analysis.
}
\value{
A list with \code{beta}, \code{cov}, \code{A}.
}
\keyword{internal}
