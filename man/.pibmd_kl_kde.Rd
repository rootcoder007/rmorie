% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.pibmd_kl_kde}
\alias{.pibmd_kl_kde}
\title{KL(q || p) from Gaussian kernel densities integrated on a grid}
\usage{
.pibmd_kl_kde(q, p, n_grid)
}
\arguments{
\item{q}{A vector; its length is taken.}
\item{p}{A vector; its length is taken.}
\item{n_grid}{A count; the body uses it as \code{seq_len(...)}.}
}
\description{
A step of the pibmd_native implementation. Called by \code{morie_pibmd_prior_informativeness_bias_diagnostic}. See the file header for the source the module follows.
}
\value{
A numeric value.
}
\keyword{internal}
\examples{
.pibmd_kl_kde(q = 0.5, p = 0.5, n_grid = c(1, 2, 3, 4, 5, 6, 7, 8))
}
