% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.ot_sinkhorn_unbalanced}
\alias{.ot_sinkhorn_unbalanced}
\title{.ot_sinkhorn_unbalanced}
\usage{
.ot_sinkhorn_unbalanced(a, b, C, eps, lam, n_iter = 200L)
}
\arguments{
\item{a}{A vector; its length is taken and its elements indexed.}
\item{b}{A vector; its length is taken and its elements indexed.}
\item{C}{Numeric; combined arithmetically in the body.}
\item{eps}{Numeric; combined arithmetically in the body.}
\item{lam}{Numeric; combined arithmetically in the body.}
\item{n_iter}{Defaults to \code{200L}.}
}
\description{
A step of the helpers_ot implementation. Called by \code{Otunbal}. See the file header for the source the module follows.
}
\value{
A numeric value.
}
\keyword{internal}
