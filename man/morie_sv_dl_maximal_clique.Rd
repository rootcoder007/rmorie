% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_sv_dl_maximal_clique}
\alias{morie_sv_dl_maximal_clique}
\title{Grow a clique from the lowest-weight edge (Section 2.1): the seed}
\usage{
morie_sv_dl_maximal_clique(members, edges)
}
\arguments{
\item{members}{See Usage.}
\item{edges}{A matrix; indexed by row and column.}
}
\description{
is e_min; then repeatedly the lowest-weight edge with exactly one endpoint inside, whose other endpoint is adjacent to every member.
}
\value{
A vector, from \code{sort}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
M <- matrix(c(1, 2, 3, 4, 5, 6), nrow = 2)
morie_sv_dl_maximal_clique(V, M)
}
