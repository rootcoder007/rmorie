% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_snpqc1}
\alias{morie_snpqc1}
\title{morie_snpqc1}
\usage{
morie_snpqc1(genotypes, phenotype = NULL, trait = "binary", geno_relaxed = 0.2, mind_relaxed = 0.2, geno = 0.02, mind = 0.02, maf_threshold = 0.01, hwe_case = 1e-10, hwe_control = 1e-06, hwe_quantitative = 1e-06, het_sd = 3, pihat = 0.2, hwe_test = "exact", x_genotypes = NULL, reported_sex = NULL, relatedness = "pihat", ibd_correction = TRUE)
}
\arguments{
\item{genotypes}{See Usage.}
\item{phenotype}{Defaults to \code{NULL}.}
\item{trait}{Defaults to \code{"binary"}.}
\item{geno_relaxed}{Defaults to \code{0.2}.}
\item{mind_relaxed}{Defaults to \code{0.2}.}
\item{geno}{Defaults to \code{0.02}.}
\item{mind}{Defaults to \code{0.02}.}
\item{maf_threshold}{Defaults to \code{0.01}.}
\item{hwe_case}{Defaults to \code{1e-10}.}
\item{hwe_control}{Defaults to \code{1e-06}.}
\item{hwe_quantitative}{Defaults to \code{1e-06}.}
\item{het_sd}{Defaults to \code{3}.}
\item{pihat}{Defaults to \code{0.2}.}
\item{hwe_test}{Defaults to \code{"exact"}.}
\item{x_genotypes}{Defaults to \code{NULL}.}
\item{reported_sex}{Defaults to \code{NULL}.}
\item{relatedness}{Defaults to \code{"pihat"}.}
\item{ibd_correction}{Defaults to \code{TRUE}.}
}
\description{
Part of the snpqc1_native implementation; see the file header for the source it follows.
}
\value{
A list with \code{estimate}, \code{keep_snps}, \code{keep_individuals}, \code{removed}, \code{n_snps_kept}, \code{n_individuals_kept}, \code{call_rate_snp}, \code{call_rate_ind}, \code{maf}, \code{hwe_p}, \code{heterozygosity}, \code{relatedness_matrix}, \code{kinship}, \code{ibd_states}, \code{relatedness}, \code{pruned_snps}, \code{thresholds}, \code{trait}, \code{hwe_test}, \code{note}, \code{method}.
}
\keyword{internal}
