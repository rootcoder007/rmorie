% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{log_prior}
\alias{log_prior}
\title{log_prior}
\usage{
log_prior(R, prior = "uniform", scale = 1, r_max = NULL, J = 0.1, H = 0, neighbours = NULL)
}
\arguments{
\item{R}{A vector; its length is taken and its elements indexed.}
\item{prior}{One of \code{"gaussian"}, \code{"laplacian"}, \code{"uniform"}. Defaults to \code{"uniform"}.}
\item{scale}{Numeric; combined arithmetically in the body. Defaults to \code{1}.}
\item{r_max}{Optional; may be \code{NULL}. Numeric; combined arithmetically in the body.}
\item{J}{Numeric; combined arithmetically in the body. Defaults to \code{0.1}.}
\item{H}{Numeric; combined arithmetically in the body. Defaults to \code{0}.}
\item{neighbours}{Defaults to \code{NULL}.}
}
\description{
A step of the birl_native implementation. Called by \code{.abcgp.alpha_terms}, \code{.abcgp.mw_sampler}, \code{abcnnt} and 1 others in the module. See the file header for the source the module follows.
}
\value{
A numeric value.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
log_prior(V)
}
