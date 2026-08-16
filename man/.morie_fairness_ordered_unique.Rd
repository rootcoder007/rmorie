% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_fairness_ordered_unique}
\alias{.morie_fairness_ordered_unique}
\title{Python\\'s "first-seen" order; unique() in R is already first-seen}
\usage{
.morie_fairness_ordered_unique(arr)
}
\arguments{
\item{arr}{Passed to \code{unique}.}
}
\description{
A step of the frns_metrics implementation. Called by \code{.morie_fairness_favorable_rates}, \code{.morie_fairness_rates_from_labels}, \code{morie_fairness_gini} and 1 others in the module. See the file header for the source the module follows.
}
\value{
The value of \code{unique}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.morie_fairness_ordered_unique(V)
}
