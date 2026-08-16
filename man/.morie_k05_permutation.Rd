% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_k05_permutation}
\alias{.morie_k05_permutation}
\title{Fisher-Yates driven by the package\\'s Philox stream, swapping}
\usage{
.morie_k05_permutation(n, seed = 0, stream = 0)
}
\arguments{
\item{n}{A count; the body uses it as \code{seq_len(...)}.}
\item{seed}{Passed to \code{.morie_random_uniform}. Defaults to \code{0}.}
\item{stream}{Passed to \code{.morie_random_uniform}. Defaults to \code{0}.}
}
\description{
downward from n-1 and consuming one uniform per step, so the Python mirror reproduces it exactly.
}
\value{
The value of \code{idx}, as built in the body.
}
\keyword{internal}
