% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_genemt_gene_covariates}
\alias{morie_genemt_gene_covariates}
\title{morie_genemt_gene_covariates}
\usage{
morie_genemt_gene_covariates(n_markers, gene_length, ld_scores = NULL)
}
\arguments{
\item{n_markers}{See Usage.}
\item{gene_length}{See Usage.}
\item{ld_scores}{Defaults to \code{NULL}.}
}
\description{
Part of the genemt_native implementation; see the file header for the source it follows.
}
\value{
A list with \code{covariates}, \code{names}, \code{note}.
}
\references{
----------
de Leeuw, C. A., Mooij, J. M., Heskes, T. & Posthuma, D. (2015)
"MAGMA: Generalized Gene-Set Analysis of GWAS Data", *PLoS
Computational Biology* 11(4), e1004219,
doi:10.1371/journal.pcbi.1004219. The stated problems with existing
gene and gene-set tools -- power strongly affected by linkage
disequilibrium between markers, multi-marker associations hard to
detect, and reliance on permutation making analysis computationally
expensive; the gene analysis based on a MULTIPLE REGRESSION model for
better statistical performance; the gene-set analysis built as a
SEPARATE LAYER around the gene analysis for flexibility; the
regression structure allowing generalisation to continuous properties
of genes and simultaneous analysis of multiple gene sets and other
gene properties; and the demonstration of more power at correct type-1
error and considerably faster analysis.

Purcell, S. et al. (2007) "PLINK: A Tool Set for Whole-Genome
Association and Population-Based Linkage Analyses", *American Journal
of Human Genetics* 81(3), 559-575, doi:10.1086/519795.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
morie_genemt_gene_covariates(V, V)
}
