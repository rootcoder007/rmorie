% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_phylby_swap_acceptance}
\alias{morie_phylby_swap_acceptance}
\title{morie_phylby_swap_acceptance}
\usage{
morie_phylby_swap_acceptance(beta_j, beta_k, logp_j, logp_k)
}
\arguments{
\item{beta_j}{Numeric; combined arithmetically in the body.}
\item{beta_k}{Numeric; combined arithmetically in the body.}
\item{logp_j}{Numeric; combined arithmetically in the body.}
\item{logp_k}{Numeric; combined arithmetically in the body.}
}
\description{
A step of the phylby_native implementation. Called by \code{morie_phylby}. See the file header for the source the module follows.
}
\value{
A numeric value.
}
\keyword{internal}
\examples{
morie_phylby_swap_acceptance(beta_j = c(1, 2, 3, 4, 5, 6, 7, 8), beta_k = c(1, 2, 3, 4, 5, 6, 7, 8), logp_j = c(1, 2, 3, 4, 5, 6, 7, 8), logp_k = c(1, 2, 3, 4, 5, 6, 7, 8))
}
