% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.karpv_roulette}
\alias{.karpv_roulette}
\title{.karpv_roulette}
\usage{
.karpv_roulette(e, adj, total)
}
\arguments{
\item{e}{Passed to \code{.karpv_below}.}
\item{adj}{A vector; its length is taken and its elements indexed.}
\item{total}{Numeric; combined arithmetically in the body.}
}
\description{
A step of the karpV_native implementation. Called by \code{morie_karpV}. See the file header for the source the module follows.
}
\value{
A numeric value.
}
\keyword{internal}
