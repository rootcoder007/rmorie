% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.frns_gini}
\alias{.frns_gini}
\title{Gini via the sorted-rank formula; equals sum_i sum_j |x_i-x_j| /}
\usage{
.frns_gini(x)
}
\arguments{
\item{x}{A vector; its length is taken.}
}
\description{
(2 n sum x). Returns 0 for all-zero or single-element input.
}
\value{
A numeric value.
}
\keyword{internal}
