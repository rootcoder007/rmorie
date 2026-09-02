% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_setT_setT}
\alias{morie_setT_setT}
\title{Set Transformer PMA_k attention pooling}
\usage{
morie_setT_setT(Z, S, params) 
}
\arguments{
\item{Z}{Numeric matrix \code{n x d}.}
\item{S}{Numeric matrix \code{k x d} of learnable seed queries.}
\item{params}{List with \code{Wq, Wk, Wv} (\code{d x d}),   \code{W1} (\code{d x d_ff}), \code{b1} (\code{d_ff}),   \code{W2} (\code{d_ff x d}), \code{b2} (\code{d}).}
}
\description{
PMA_k(Z) = MAB(S, rFF(Z)) (Lee et al. 2019, Eq 7 + Sec 3.2). Because every row of Z enters only through K and V of the pooling attention, the output is PERMUTATION INVARIANT in the set elements.
}
\value{
List with \code{output} (k x d), \code{attention} (k x n),   \code{k}, \code{estimate} (first output element), \code{n},   \code{method}.
}
\keyword{internal}
