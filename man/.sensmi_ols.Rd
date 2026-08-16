% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.sensmi_ols}
\alias{.sensmi_ols}
\title{Internal: least squares with an intercept prepended. Returns the}
\usage{
.sensmi_ols(X, y)
}
\arguments{
\item{X}{See Usage.}
\item{y}{Numeric; combined arithmetically in the body.}
}
\description{
coefficients and the residuals in one list so callers do not refit.
}
\value{
A list with \code{beta}, \code{resid}.
}
\keyword{internal}
