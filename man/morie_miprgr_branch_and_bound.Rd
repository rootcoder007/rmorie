% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_miprgr_branch_and_bound}
\alias{morie_miprgr_branch_and_bound}
\title{Branch and bound (Dakin 1965 Fig. 2, marked list)}
\usage{
morie_miprgr_branch_and_bound(A, b, c, integer_vars, maximise = TRUE, prune = TRUE, max_nodes = 5000L, solver = "simplex")
}
\arguments{
\item{A,b,c,integer_vars,maximise}{As above.}
\item{prune}{Apply the bound test.}
\item{max_nodes}{Hard cap on relaxation solves.}
\item{solver}{"simplex" or "interior".}
}
\description{
Branch and bound (Dakin 1965 Fig. 2, marked list)
}
\value{
A list with the incumbent, the relaxation node count, the   number of nodes pruned, the maximum length of the marked list,   and the root relaxation value.
}
\keyword{internal}
\examples{
morie_miprgr_branch_and_bound(A = c(1, 2, 3, 4, 5, 6, 7, 8), b = c(1, 2, 3, 4, 5, 6, 7, 8), c = c(1, 2, 3, 4, 5, 6, 7, 8), integer_vars = c("a", "b", "c"))
}
