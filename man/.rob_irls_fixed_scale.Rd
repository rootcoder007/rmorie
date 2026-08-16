% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.rob_irls_fixed_scale}
\alias{.rob_irls_fixed_scale}
\title{.rob_irls_fixed_scale}
\usage{
.rob_irls_fixed_scale(X, y, beta, scale, cc)
}
\arguments{
\item{X}{A matrix; passed to \code{\%*\%}.}
\item{y}{A matrix; passed to \code{crossprod}.}
\item{beta}{A matrix; passed to \code{\%*\%}.}
\item{scale}{Numeric; combined arithmetically in the body.}
\item{cc}{Passed to \code{.rob_tukey_w}.}
}
\description{
A step of the robust_native implementation. Called by \code{morie_rob_mm}. See the file header for the source the module follows.
}
\value{
A list with \code{beta}, \code{converged}.
}
\keyword{internal}
