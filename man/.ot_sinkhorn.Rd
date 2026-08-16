% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.ot_sinkhorn}
\alias{.ot_sinkhorn}
\title{.ot_sinkhorn}
\usage{
.ot_sinkhorn(a, b, C, eps, n_iter = 200L)
}
\arguments{
\item{a}{A vector; its length is taken.}
\item{b}{A vector; its length is taken.}
\item{C}{A matrix; indexed by row and column.}
\item{eps}{Numeric; combined arithmetically in the body.}
\item{n_iter}{Coerced to integer by the body, with \code{as.integer}. Defaults to \code{200L}.}
}
\description{
A step of the helpers_ot implementation. Called by \code{Otdiv}, \code{Otgws}, \code{Otker} and 2 others in the module. See the file header for the source the module follows.
}
\value{
A list with \code{T}, \code{f}, \code{g}.
}
\keyword{internal}
