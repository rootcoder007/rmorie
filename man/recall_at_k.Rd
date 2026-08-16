% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{recall_at_k}
\alias{recall_at_k}
\title{Recall at k}
\usage{
recall_at_k(ranked, target, kk = 20L)
}
\arguments{
\item{ranked}{Integer vector of ranked items.}
\item{target}{Target item.}
\item{kk}{Cutoff.}
}
\description{
Recall at k
}
\value{
1 if target in top kk, else 0.
}
\keyword{internal}
