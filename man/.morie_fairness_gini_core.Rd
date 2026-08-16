% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_fairness_gini_core}
\alias{.morie_fairness_gini_core}
\title{Sorted-rank formula. Returns 0.0 for an all-zero or single-element}
\usage{
.morie_fairness_gini_core(x)
}
\arguments{
\item{x}{A vector; its length is taken.}
}
\description{
input (no inequality defined), matching the Python helper.
}
\value{
A numeric value.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.morie_fairness_gini_core(V)
}
