% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_geron_roc_curve_hm}
\alias{morie_geron_roc_curve_hm}
\title{ROC curve: FPR vs TPR over thresholds, trapezoid area cross-check (Geron Ch 3, hmroc)}
\usage{
morie_geron_roc_curve_hm(y_true, scores, pos_label = 1) 
}
\arguments{
\item{y_true}{Binary labels.}
\item{scores}{Decision scores.}
\item{pos_label}{Positive label.}
}
\description{
ROC curve: FPR vs TPR over thresholds, trapezoid area cross-check (Geron Ch 3, hmroc)
}
\value{
A list with \code{fpr}, \code{tpr}, \code{thresholds}, \code{auc}, \code{auc_trapezoid}, \code{youden_j}, \code{best_threshold}, \code{n_pos}, \code{n_neg}, \code{estimate}, \code{n}, \code{method}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
morie_geron_roc_curve_hm(V, V)
}
