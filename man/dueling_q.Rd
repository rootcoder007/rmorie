% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{dueling_q}
\alias{dueling_q}
\title{Duel aggregation across a batch of states}
\usage{
dueling_q(values, advantages, mode = "mean")
}
\arguments{
\item{values}{Numeric vector of state values V(s).}
\item{advantages}{List of numeric advantage vectors A(s, .), one   per state.}
\item{mode}{One of \code{"mean"}, \code{"max"}, \code{"naive"}.}
}
\description{
Applies \code{\link{dueling_aggregate}} row-wise.
}
\value{
List of numeric Q(s, .) vectors.
}
\keyword{internal}
