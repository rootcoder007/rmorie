% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{Roc}
\alias{Roc}
\title{Section 10.9.1.  The area the book calls A_z, by the trapezoidal}
\usage{
Roc(scores, labels, positive = 1)
}
\arguments{
\item{scores}{See Usage.}
\item{labels}{See Usage.}
\item{positive}{Defaults to \code{1}.}
}
\description{
rule, which for tied scores is exactly the Mann-Whitney statistic -- the probability a random diseased subject outscores a random healthy one, ties counted as half.  Summing rectangles instead biases the area whenever scores tie, which they do whenever a classifier emits a class rather than a probability.
}
\value{
A list with \code{fpf}, \code{tpf}, \code{sensitivity}, \code{one_minus_specificity}, \code{thresholds}, \code{auc}, \code{az}, \code{mann_whitney}, \code{trapezoidal_equals_mann_whitney}, \code{n_positive}, \code{n_negative}, \code{best_index}, \code{best_operating_point}, \code{ties_counted_as_half}, \code{method}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
Roc(V, V)
}
