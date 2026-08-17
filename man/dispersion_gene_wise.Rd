% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{dispersion_gene_wise}
\alias{dispersion_gene_wise}
\title{Gene-wise dispersion estimate}
\usage{
dispersion_gene_wise(K, X, s, alpha_init = 0.1)
}
\arguments{
\item{K}{Count vector for one gene.}
\item{X}{Design matrix.}
\item{s}{Size factors.}
\item{alpha_init}{Initial dispersion.}
}
\description{
Maximises equation 7 with the fitted values from an initial GLM held fixed, exactly as the paper describes.
}
\value{
A list with \code{dispersion} and \code{mu0}.
}
\keyword{internal}
\examples{
set.seed(2)
m <- 6
X <- cbind(1, rep(c(0, 1), each = 3))
K <- c(rnbinom(3, mu = 50, size = 10), rnbinom(3, mu = 90, size = 10))
dispersion_gene_wise(K, X, s = rep(1, m))
}
