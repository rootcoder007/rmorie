% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.mor_cp_tables}
\alias{.mor_cp_tables}
\title{Cumulative sums; .mor_cp_cs(x)$cs[k] is Python cs[k - 1]}
\usage{
.mor_cp_tables(x)
}
\arguments{
\item{x}{A vector; its length is taken and its elements indexed.}
}
\description{
A step of the pelt_native implementation. Called by \code{.mor_pelt_core}, \code{morie_binseg}. See the file header for the source the module follows.
}
\value{
A list with \code{cs}, \code{css}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.mor_cp_tables(V)
}
