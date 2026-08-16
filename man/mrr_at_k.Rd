% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{mrr_at_k}
\alias{mrr_at_k}
\title{MRR at k}
\usage{
mrr_at_k(ranked, target, kk = 20L)
}
\description{
MRR at k
}
\value{
Reciprocal rank, 0 if not in top k.
}
\keyword{internal}
\examples{
mrr_at_k(ranked = c(1, 2, 3, 4, 5, 6, 7, 8), target = 5L)
}
