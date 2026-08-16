% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_phylml}
\alias{morie_phylml}
\title{morie_phylml}
\usage{
morie_phylml(tree, seqs, pi = NULL, u = 1)
}
\arguments{
\item{tree}{Passed to \code{site_likelihood}.}
\item{seqs}{A vector; its length is taken.}
\item{pi}{Passed to \code{.phylml_pi}.}
\item{u}{Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{1}.}
}
\description{
A step of the phylml_native implementation. Called by \code{.phylby_log_posterior}, \code{optimise_branch}. See the file header for the source the module follows.
}
\value{
The value of \code{result}, as built in the body.
}
\keyword{internal}
\examples{
morie_phylml(tree = list(a = 1, b = 2), seqs = c("a", "b", "c"))
}
