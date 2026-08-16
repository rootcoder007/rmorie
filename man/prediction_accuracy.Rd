% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{prediction_accuracy}
\alias{prediction_accuracy}
\title{prediction_accuracy}
\usage{
prediction_accuracy(predicted, observed, fold = 2)
}
\arguments{
\item{predicted}{See Usage.}
\item{observed}{See Usage.}
\item{fold}{Defaults to \code{2}.}
}
\description{
Part of the clrnt_native implementation; see the file header for the source it follows.
}
\value{
A list with \code{afe}, \code{fold_underprediction}, \code{rmse}, \code{esf}, \code{average_esf}, \code{within_fold}, \code{beyond_fold}, \code{n}, \code{fold}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
prediction_accuracy(V, V)
}
