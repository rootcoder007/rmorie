% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.abcgp.summarise}
\alias{.abcgp.summarise}
\title{.abcgp.summarise}
\usage{
.abcgp.summarise(x, summary)
}
\arguments{
\item{x}{See Usage.}
\item{summary}{See Usage.}
}
\description{
A step of the abcgp_native implementation. Called by \code{.abcgp.gabc_log_likelihood}, \code{.abcgp.mw_sampler}, \code{.abcgp.synthetic_log_likelihood}. See the file header for the source the module follows.
}
\value{
A vector, from \code{as.numeric}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.abcgp.summarise(V, V)
}
