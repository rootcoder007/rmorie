% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_snpqc1_kinship_matrix}
\alias{morie_snpqc1_kinship_matrix}
\title{Genomic kinship from centred, scaled genotypes:}
\usage{
morie_snpqc1_kinship_matrix(genotypes)
}
\arguments{
\item{genotypes}{Passed to \code{.snpqc1_check}.}
}
\description{
K_ik = (1/M) sum_j (g_ij - 2p_j)(g_kj - 2p_j) / (2 p_j (1 - p_j)). On the same scale as pi-hat but NOT PLINK\'s pi-hat.
}
\value{
The value of \code{K}, as built in the body.
}
\keyword{internal}
\examples{
morie_snpqc1_kinship_matrix(genotypes = list(a = 1, b = 2))
}
