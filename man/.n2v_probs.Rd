% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.n2v_probs}
\alias{.n2v_probs}
\title{Unnormalised second-order weights out of cur, having come from prev}
\usage{
.n2v_probs(nb, prev, cur, p, q)
}
\arguments{
\item{nb}{A vector; indexed elementwise.}
\item{prev}{See Usage.}
\item{cur}{See Usage.}
\item{p}{Numeric; combined arithmetically in the body.}
\item{q}{Numeric; combined arithmetically in the body.}
}
\description{
1/p to step back, 1 to a neighbour of prev, 1/q otherwise.  At p = q = 1 every weight is 1, so the walk is exactly DeepWalk\\'s uniform walk -- the reduction that pins this function.
}
\value{
The value of \code{out}, as built in the body.
}
\keyword{internal}
