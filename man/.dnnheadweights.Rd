% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.dnnheadweights}
\alias{.dnnheadweights}
\title{The p.493 median/quantile recipe}
\usage{
.dnnheadweights(Yc, nt, n)
}
\arguments{
\item{Yc}{A matrix; indexed by row and column.}
\item{nt}{A count; the body uses it as \code{seq_len(...)}.}
\item{n}{Accepted by the signature and not used anywhere in the body.}
}
\description{
A step of the dnnmt implementation. Called by \code{Dnnmt}. See the file header for the source the module follows.
}
\value{
A vector, from \code{vapply}.
}
\keyword{internal}
