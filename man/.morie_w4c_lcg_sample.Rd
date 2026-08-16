% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_w4c_lcg_sample}
\alias{.morie_w4c_lcg_sample}
\title{Fisher-Yates partial shuffle drawing k of n indices (0-based out),}
\usage{
.morie_w4c_lcg_sample(n, k, seed)
}
\arguments{
\item{n}{Numeric; combined arithmetically in the body.}
\item{k}{A count; the body uses it as \code{seq_len(...)}.}
\item{seed}{Coerced to numeric by the body, with \code{as.numeric}.}
}
\description{
LCG-seeded Part of the geron_w4c_native implementation; see the file header for the source it follows.
}
\value{
A vector, from \code{sort}.
}
\keyword{internal}
