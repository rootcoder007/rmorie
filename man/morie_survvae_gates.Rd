% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_survvae_gates}
\alias{morie_survvae_gates}
\title{Softmax over the experts}
\usage{
morie_survvae_gates(x, W, bias)
}
\arguments{
\item{x}{Covariate vector of length \code{d}.}
\item{W}{List of \code{K} numeric weight vectors, each of length   \code{d}.}
\item{bias}{Numeric vector of length \code{K}.}
}
\description{
Numerical-stable softmax of the gate logits \eqn{z_k = W_k x + b_k}.
}
\value{
Numeric vector of length \code{K}, sums to one.
}
\keyword{internal}
