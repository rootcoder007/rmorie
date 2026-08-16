% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{SepIndex}
\alias{SepIndex}
\title{Section 10.10.1: J = tr(S_B) / tr(S_W).  The trace ratio ignores the}
\usage{
SepIndex(X, y)
}
\arguments{
\item{X}{A matrix; passed to \code{as.matrix}.}
\item{y}{A vector; its length is taken.}
}
\description{
OFF-diagonal structure, so it cannot see that a pair of features is jointly discriminating when neither is alone -- for that, the divergence, which uses the full covariance, is the measure to use.
}
\value{
A list with \code{j}, \code{trace_between}, \code{trace_within}, \code{s_within}, \code{s_between}, \code{classes}, \code{n_classes}, \code{n_features}, \code{ignores_off_diagonal_structure}, \code{method}.
}
\keyword{internal}
\examples{
SepIndex(matrix(c(0, 0, 0.1, 0, 1, 0, 1.1, 0), ncol = 2, byrow = TRUE), 
    c(0, 0, 1, 1))
}
