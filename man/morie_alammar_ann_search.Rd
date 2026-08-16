% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_alammar_ann_search}
\alias{morie_alammar_ann_search}
\title{Greedy NSW approximate nearest neighbour (Malkov and Yashunin 2020)}
\usage{
morie_alammar_ann_search(query_vec, index, ef_search = 8)
}
\arguments{
\item{query_vec}{Query.}
\item{index}{List with points, neighbors   (0-based lists), entry (0-based).}
\item{ef_search}{Beam budget.}
}
\description{
Greedy NSW approximate nearest neighbour (Malkov and Yashunin 2020)
}
\value{
A list with \code{nearest}, \code{distance}, \code{greedy_path}, \code{candidates_examined}, \code{exact_nearest}, \code{found_exact}, \code{estimate}, \code{n}, \code{method}.
}
\keyword{internal}
