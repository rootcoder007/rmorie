% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.limmav_ols}
\alias{.limmav_ols}
\title{.limmav_ols}
\usage{
.limmav_ols(X, y, w = NULL)
}
\arguments{
\item{X}{A matrix; passed to \code{ncol}.}
\item{y}{A matrix; passed to \code{crossprod}.}
\item{w}{Optional; may be \code{NULL}. Coerced to numeric by the body, with \code{as.numeric}.}
}
\description{
A step of the limmav_native implementation. Called by \code{.limmav_voom_weights}, \code{.limmav_weighted_lm}. See the file header for the source the module follows.
}
\value{
A list with \code{beta}, \code{fit}, \code{sd}, \code{inv}, \code{df}.
}
\keyword{internal}
