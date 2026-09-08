% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_gwasem_gower}
\alias{morie_gwasem_gower}
\title{Gower centring (equation 5)}
\usage{
morie_gwasem_gower(S) 
}
\arguments{
\item{S}{n x n matrix.}
}
\description{
Scales the relatedness matrix to sample variance 1 so that \code{sigma_a^2} is on the scale of the phenotypic variance.
}
\value{
Centred matrix.
}
\keyword{internal}
\examples{
M <- matrix(c(1, 2, 3, 4, 5, 6), nrow = 2)
morie_gwasem_gower(M)
}
