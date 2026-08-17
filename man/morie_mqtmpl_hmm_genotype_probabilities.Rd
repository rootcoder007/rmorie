% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_mqtmpl_hmm_genotype_probabilities}
\alias{morie_mqtmpl_hmm_genotype_probabilities}
\title{Forward-backward posterior genotype probabilities}
\usage{
morie_mqtmpl_hmm_genotype_probabilities(genotypes, positions, error_rate = 0)
}
\arguments{
\item{genotypes}{One row per individual: 0, 1 or NA.}
\item{positions}{Marker positions in cM.}
\item{error_rate}{Genotyping error rate in [0, 0.5).}
}
\description{
Forward-backward posterior genotype probabilities
}
\value{
A list of per-individual posterior matrices.
}
\keyword{internal}
\examples{
set.seed(1)
g <- lapply(1:6, function(i) c(rbinom(1, 1, 0.5), NA, rbinom(1, 1, 0.5)))
pos <- c(0, 0.1, 0.2)
r <- morie_mqtmpl_hmm_genotype_probabilities(g, pos)
str(r[[1]])
}
