% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_fairness_rates_from_labels}
\alias{.morie_fairness_rates_from_labels}
\title{.morie_fairness_rates_from_labels}
\usage{
.morie_fairness_rates_from_labels(y_true, y_pred, group, favorable)
}
\arguments{
\item{y_true}{A vector; indexed elementwise.}
\item{y_pred}{A vector; indexed elementwise.}
\item{group}{Passed to \code{.morie_fairness_ordered_unique}.}
\item{favorable}{Passed to \code{==}.}
}
\description{
A step of the frns_metrics implementation. Called by \code{morie_fairness_average_odds_difference}, \code{morie_fairness_equalized_odds}. See the file header for the source the module follows.
}
\value{
The value of \code{out}, as built in the body.
}
\keyword{internal}
\examples{
.morie_fairness_rates_from_labels(y_true = c(1, 2, 3, 4, 5, 6, 7, 8), y_pred = c(1, 2, 3, 4, 5, 6, 7, 8), group = c("a", "b", "c"), favorable = c(1, 2, 3, 4, 5, 6, 7, 8))
}
