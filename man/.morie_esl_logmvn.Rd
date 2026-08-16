% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_esl_logmvn}
\alias{.morie_esl_logmvn}
\title{.morie_esl_logmvn}
\usage{
.morie_esl_logmvn(X, mu, S)
}
\arguments{
\item{X}{A matrix; passed to \code{ncol}.}
\item{mu}{See Usage.}
\item{S}{A matrix; passed to \code{chol}.}
}
\description{
A step of the esl_native implementation. Called by \code{morie_esl_em_gmm}, \code{morie_esl_gaussian_mixture}. See the file header for the source the module follows.
}
\value{
A numeric value.
}
\keyword{internal}
\examples{
.morie_esl_logmvn(X = matrix(c(1, 2, 3, 4, 5, 6), nrow = 2), mu = c(1, 2, 3, 4, 5, 6, 7, 8), S = 5L)
}
