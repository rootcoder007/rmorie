% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{precision_recall}
\alias{precision_recall}
\title{precision_recall}
\usage{
precision_recall(pred, truth)
}
\arguments{
\item{pred}{See Usage.}
\item{truth}{See Usage.}
}
\description{
Part of the memb_native implementation; see the file header for the source it follows.
}
\value{
A list with \code{precision}, \code{recall}, \code{accuracy}, \code{tp}, \code{fp}, \code{fn}, \code{tn}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
precision_recall(V, V)
}
