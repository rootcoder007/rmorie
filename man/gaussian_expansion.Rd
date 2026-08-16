% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{gaussian_expansion}
\alias{gaussian_expansion}
\title{gaussian_expansion}
\usage{
gaussian_expansion(r, mu_min = 0, mu_max = 6, n_gaussians = 25, gamma = NULL)
}
\arguments{
\item{r}{See Usage.}
\item{mu_min}{Defaults to \code{0}.}
\item{mu_max}{Defaults to \code{6}.}
\item{n_gaussians}{Defaults to \code{25}.}
\item{gamma}{Defaults to \code{NULL}.}
}
\description{
Part of the schN_native implementation; see the file header for the source it follows.
}
\value{
A numeric value.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
gaussian_expansion(V)
}
