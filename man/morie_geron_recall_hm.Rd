% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_geron_recall_hm}
\alias{morie_geron_recall_hm}
\title{Recall (true positive rate) = TP / (TP + FN) (Geron Ch 3, hmrec)}
\usage{
morie_geron_recall_hm(y_true, y_pred, pos_label = 1) 
}
\arguments{
\item{y_true,y_pred}{Labels, same length.}
\item{pos_label}{Positive label.}
}
\description{
Recall (true positive rate) = TP / (TP + FN) (Geron Ch 3, hmrec)
}
\value{
A list with \code{recall}, \code{tp}, \code{fn}, \code{fp}, \code{f1}, \code{estimate}, \code{n}, \code{method}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
morie_geron_recall_hm(V, V)
}
