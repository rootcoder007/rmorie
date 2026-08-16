% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.t4_acfbiased}
\alias{.t4_acfbiased}
\title{Sample autocorrelations with the biased (n in both numerator and}
\usage{
.t4_acfbiased(x, lag)
}
\arguments{
\item{x}{A vector; its length is taken.}
\item{lag}{A count; the body uses it as \code{seq_len(...)}.}
}
\description{
denominator) normalisation, i.e. stats::acf.
}
\value{
A vector, from \code{vapply}.
}
\keyword{internal}
