% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{site_likelihood}
\alias{site_likelihood}
\title{site_likelihood}
\usage{
site_likelihood(tree, seqs, site, pi = NULL, u = 1)
}
\arguments{
\item{tree}{Passed to \code{.phylml_prune}.}
\item{seqs}{Passed to \code{.phylml_prune}.}
\item{site}{Passed to \code{.phylml_prune}.}
\item{pi}{Passed to \code{.phylml_pi}.}
\item{u}{Passed to \code{.phylml_prune}. Defaults to \code{1}.}
}
\description{
A step of the phylml_native implementation. Called by \code{morie_phylml}. See the file header for the source the module follows.
}
\value{
A numeric value.
}
\keyword{internal}
\examples{
site_likelihood(tree = list(a = 1, b = 2), seqs = c("a", "b", "c"), site = c(1, 2, 3, 4, 5, 6, 7, 8))
}
