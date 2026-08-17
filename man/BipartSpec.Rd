% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{BipartSpec}
\alias{BipartSpec}
\title{\\' Decide bipartiteness, with the odd power-traces as evidence}
\usage{
BipartSpec(A)
}
\arguments{
\item{A}{A matrix; passed to \code{nrow}.}
}
\description{
\\' \\' A symmetric A has a spectrum symmetric about zero exactly when \\' trace(A^k) = 0 for every odd k, and trace(A^k) counts closed walks of \\' length k, so the criterion is "no odd cycle".  Both the spectral and \\' the two-colouring answer are computed and must agree. \\' \\' @param A symmetric adjacency matrix with a zero diagonal \\' @return list(bipartite, evidence, max_odd_trace, colouring, \\' part_sizes, n_components, n, m, method) \\' @export
}
\value{
A list with \code{bipartite}, \code{evidence}, \code{max_odd_trace}, \code{colouring}, \code{part_sizes}, \code{n_components}, \code{n}, \code{m}, \code{method}.
}
\keyword{internal}
\examples{
A <- rbind(c(0, 1, 0, 0), c(1, 0, 1, 0), c(0, 1, 0, 1), c(0, 0, 1, 0))
r <- BipartSpec(A)
str(r, max.level = 1)
}
