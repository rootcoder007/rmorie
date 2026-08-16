% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.hrz3_wquant}
\alias{.hrz3_wquant}
\title{Weighted tau-quantile: the smallest order statistic of v whose}
\usage{
.hrz3_wquant(v, w, tau)
}
\arguments{
\item{v}{A vector; its length is taken and its elements indexed.}
\item{w}{A vector; its length is taken and its elements indexed.}
\item{tau}{Numeric; combined arithmetically in the body.}
}
\description{
cumulative normalised weight reaches tau.  With equal weights this is the usual empirical quantile, so it agrees with a plain sort on a degenerate kernel -- the anchor used by Hrzplrq.
}
\value{
The value of \code{[}.
}
\keyword{internal}
