% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.crp_norm_logpdf}
\alias{.crp_norm_logpdf}
\title{.crp_norm_logpdf}
\usage{
.crp_norm_logpdf(x, mu, var)
}
\arguments{
\item{x}{Numeric; combined arithmetically in the body.}
\item{mu}{Numeric; combined arithmetically in the body.}
\item{var}{Numeric; combined arithmetically in the body.}
}
\description{
A step of the crpcol implementation. Called by \code{.crp_collapsed_sweep}, \code{Crpcol}, \code{Crpgib} and 1 others in the module. See the file header for the source the module follows.
}
\value{
A numeric value.
}
\keyword{internal}
\examples{
.crp_norm_logpdf(x = c(1, 2, 3, 4, 5, 6, 7, 8), mu = c(1, 2, 3, 4, 5, 6, 7, 8), var = c(1, 2, 3, 4, 5, 6, 7, 8))
}
