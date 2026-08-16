% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.limmav_weighted_lm}
\alias{.limmav_weighted_lm}
\title{.limmav_weighted_lm}
\usage{
.limmav_weighted_lm(y, X, w, contrast)
}
\arguments{
\item{y}{Passed to \code{.limmav_ols}.}
\item{X}{Passed to \code{.limmav_ols}.}
\item{w}{Passed to \code{.limmav_ols}.}
\item{contrast}{Coerced to numeric by the body, with \code{as.numeric}.}
}
\description{
A step of the limmav_native implementation. Called by \code{morie_limmav}. See the file header for the source the module follows.
}
\value{
A list with \code{est}, \code{se}, \code{t}, \code{df}, \code{sd}, \code{v_un}.
}
\keyword{internal}
\examples{
.limmav_weighted_lm(y = c(1, 2, 3, 4, 5, 6, 7, 8), X = c(1, 2, 3, 4, 5, 6, 7, 8), w = c(1, 2, 3, 4, 5, 6, 7, 8), contrast = c(1, 2, 3, 4, 5, 6, 7, 8))
}
