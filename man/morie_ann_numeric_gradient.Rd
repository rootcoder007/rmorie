% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_ann_numeric_gradient}
\alias{morie_ann_numeric_gradient}
\title{morie_ann_numeric_gradient}
\usage{
morie_ann_numeric_gradient(X, y, W, activations = NULL, eps = 1e-06) 
}
\description{
morie_ann_numeric_gradient
}
\arguments{
\item{X}{Argument \code{X}; see Usage.}
\item{y}{Argument \code{y}; see Usage.}
\item{W}{Argument \code{W}; see Usage.}
\item{activations}{Argument \code{activations}; see Usage.}
\item{eps}{Argument \code{eps}; see Usage.}
}
\value{
The value of \code{lapply}.
}
\keyword{internal}
\examples{
morie_ann_numeric_gradient(X = c(1, 2, 3, 4, 5, 6, 7, 8), y = c(1, 2, 3, 4, 5, 6, 7, 8), W = c(1, 2, 3, 4, 5, 6, 7, 8))
}
