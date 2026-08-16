% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_alammar_multiple_negatives_ranking}
\alias{morie_alammar_multiple_negatives_ranking}
\title{morie_alammar_multiple_negatives_ranking}
\usage{
morie_alammar_multiple_negatives_ranking(anchors, positives, tau = 0.05)
}
\arguments{
\item{anchors,positives}{Row-matched batches; every other positive   is an in-batch negative.}
}
\description{
morie_alammar_multiple_negatives_ranking
}
\value{
A list with \code{estimate}, \code{losses}, \code{similarity_matrix}, \code{n}, \code{method}.
}
\keyword{internal}
