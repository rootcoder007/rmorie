% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.cmlmer_upgma}
\alias{.cmlmer_upgma}
\title{Average-linkage clustering on 1 - K, cut at g groups. Deterministic}
\usage{
.cmlmer_upgma(K, g)
}
\arguments{
\item{K}{A matrix; passed to \code{nrow}.}
\item{g}{See Usage.}
}
\description{
throughout: ties are broken by the smaller index pair, so two implementations agree on the dendrogram, not merely on its quality.
}
\value{
A list with \code{lab}, \code{groups}.
}
\keyword{internal}
