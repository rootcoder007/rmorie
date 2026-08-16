% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.esl_matrix}
\alias{.esl_matrix}
\title{.esl_matrix}
\usage{
.esl_matrix(X, y, what = "X")
}
\arguments{
\item{X}{A matrix; passed to \code{as.matrix}.}
\item{y}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{what}{Defaults to \code{"X"}.}
}
\description{
A step of the esl_native0 implementation. Called by \code{morie_esl_bootstrap_err}, \code{morie_esl_random_forest}, \code{morie_esl_residual_variance}. See the file header for the source the module follows.
}
\value{
A list with \code{X}, \code{y}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.esl_matrix(V, V)
}
