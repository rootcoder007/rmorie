% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_gwasem_kinship}
\alias{morie_gwasem_kinship}
\title{IBS relatedness matrix}
\usage{
morie_gwasem_kinship(genotypes)
}
\arguments{
\item{genotypes}{n x m matrix of minor-allele counts.}
}
\description{
Mean proportion of alleles shared identical by state across markers, one of the matrices the paper names for step 1.
}
\value{
n x n symmetric matrix.
}
\keyword{internal}
