% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.mor_cp_cost}
\alias{.mor_cp_cost}
\title{Segment cost of the 0-based half-open block [a, b)}
\usage{
.mor_cp_cost(tab, cost)
}
\arguments{
\item{tab}{A list; the body reads \code{$cs}, \code{$css} from it.}
\item{cost}{One of \code{"mean"}, \code{"meanvar"}.}
}
\description{
A step of the pelt_native implementation. Called by \code{.mor_pelt_core}, \code{morie_binseg}. See the file header for the source the module follows.
}
\value{
The value of \code{function}.
}
\keyword{internal}
