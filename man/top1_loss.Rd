% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{top1_loss}
\alias{top1_loss}
\title{TOP1 ranking loss}
\usage{
top1_loss(r_target, r_negatives, regularize = TRUE)
}
\arguments{
\item{r_target}{Target score.}
\item{r_negatives}{Vector of negative scores.}
\item{regularize}{If TRUE, include the sigma(r_neg^2) term.}
}
\description{
TOP1 ranking loss
}
\value{
Scalar loss.
}
\keyword{internal}
