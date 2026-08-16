% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_w4c_pr_curve}
\alias{.morie_w4c_pr_curve}
\title{Standard (sklearn-style) precision-recall curve: sweep in descending}
\usage{
.morie_w4c_pr_curve(y_bin, s)
}
\arguments{
\item{y_bin}{A vector; indexed elementwise.}
\item{s}{A vector; indexed elementwise.}
}
\description{
score order, one vertex per distinct score, leading (recall=0, precision=1) point from a threshold above every score, step-function average precision.
}
\value{
A list with \code{precision}, \code{recall}, \code{average_precision}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.morie_w4c_pr_curve(V, V)
}
