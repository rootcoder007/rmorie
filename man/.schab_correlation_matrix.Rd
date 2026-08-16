% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.schab_correlation_matrix}
\alias{.schab_correlation_matrix}
\title{Sigma(theta*) = xi I + (1 - xi) R(h; a). Factoring}
\usage{
.schab_correlation_matrix(coords, nugget_ratio, rng, model)
}
\arguments{
\item{coords}{See Usage.}
\item{nugget_ratio}{See Usage.}
\item{rng}{See Usage.}
\item{model}{See Usage.}
}
\description{
sigma^2 = c0 + sigma0^2 out of Sigma leaves the nugget as a RATIO in [0, 1] -- the reparameterisation Sec. 5.5.2 calls for.
}
\value{
A list with \code{sigma}, \code{d}, \code{r}.
}
\keyword{internal}
