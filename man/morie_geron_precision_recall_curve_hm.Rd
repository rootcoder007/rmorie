% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_geron_precision_recall_curve_hm}
\alias{morie_geron_precision_recall_curve_hm}
\title{Precision-recall curve: best-F1 point and recall at 90\% precision (Geron Ch 3, hmprc)}
\usage{
morie_geron_precision_recall_curve_hm(y_true, scores, pos_label = 1)
}
\arguments{
\item{y_true}{Binary labels.}
\item{scores}{Decision scores.}
\item{pos_label}{Positive label.}
}
\description{
Precision-recall curve: best-F1 point and recall at 90\% precision (Geron Ch 3, hmprc)
}
\value{
A list with \code{precision}, \code{recall}, \code{thresholds}, \code{average_precision}, \code{f1}, \code{best_f1}, \code{best_threshold}, \code{recall_at_90_precision}, \code{estimate}, \code{n}, \code{method}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
morie_geron_precision_recall_curve_hm(V, V)
}
