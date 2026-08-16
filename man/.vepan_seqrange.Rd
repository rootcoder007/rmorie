% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.vepan_seqrange}
\alias{.vepan_seqrange}
\title{Python range(lo, hi+1); empty when hi < lo}
\usage{
.vepan_seqrange(lo, hi)
}
\arguments{
\item{lo}{See Usage.}
\item{hi}{See Usage.}
}
\description{
A step of the vepan_native implementation. Called by \code{.vepan_apply}, \code{.vepan_hgvs_c}, \code{.vepan_splice_terms} and 1 others in the module. See the file header for the source the module follows.
}
\value{
One of two values, depending on the branch taken.
}
\keyword{internal}
