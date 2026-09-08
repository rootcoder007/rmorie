% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_geron_precision_hm}
\alias{morie_geron_precision_hm}
\title{Precision = TP / (TP + FP) (Geron Ch 3, hmpre)}
\usage{
morie_geron_precision_hm(y_true, y_pred, pos_label = 1) 
}
\arguments{
\item{y_true,y_pred}{Labels, same length.}
\item{pos_label}{Positive label.}
}
\description{
Precision = TP / (TP + FP) (Geron Ch 3, hmpre)
}
\value{
A list with \code{precision}, \code{tp}, \code{fp}, \code{fn}, \code{f1}, \code{estimate}, \code{n}, \code{method}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
morie_geron_precision_hm(V, V)
}
