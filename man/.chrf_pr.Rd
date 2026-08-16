% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.chrf_pr}
\alias{.chrf_pr}
\title{.chrf_pr}
\usage{
.chrf_pr(hyp_grams, ref_grams)
}
\arguments{
\item{hyp_grams}{A vector; its length is taken.}
\item{ref_grams}{A vector; its length is taken.}
}
\description{
A step of the chrF_native implementation. Called by \code{chrf_score}. See the file header for the source the module follows.
}
\value{
A vector, from \code{c}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.chrf_pr(V, V)
}
