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
\item{genotypes}{Passed to \code{.snpqc1_check}.}
\item{phenotype}{Optional; may be \code{NULL}. Coerced to numeric by the body, with \code{as.numeric}.}
\item{trait}{One of \code{"binary"}, \code{"quantitative"}. Defaults to \code{"binary"}.}
\item{geno_relaxed}{Defaults to \code{0.2}.}
\item{mind_relaxed}{Defaults to \code{0.2}.}
\item{geno}{Defaults to \code{0.02}.}
\item{mind}{Defaults to \code{0.02}.}
\item{maf_threshold}{Defaults to \code{0.01}.}
\item{hwe_case}{Defaults to \code{1e-10}.}
\item{hwe_control}{Defaults to \code{1e-06}.}
\item{hwe_quantitative}{Defaults to \code{1e-06}.}
\item{het_sd}{Numeric; combined arithmetically in the body. Defaults to \code{3}.}
\item{pihat}{Defaults to \code{0.2}.}
\item{hwe_test}{Passed to \code{morie_snpqc1_hwe_pvalue}. Defaults to \code{"exact"}.}
\item{x_genotypes}{Optional; may be \code{NULL}. A matrix; passed to \code{as.matrix}.}
\item{reported_sex}{Optional; may be \code{NULL}. Coerced to integer by the body, with \code{as.integer}.}
\item{relatedness}{One of \code{"kinship"}, \code{"pihat"}. Defaults to \code{"pihat"}.}
\item{ibd_correction}{Passed to \code{morie_snpqc1_ibd_moments}. Defaults to \code{TRUE}.}
}
\description{
A step of the snpqc1_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A list with \code{estimate}, \code{keep_snps}, \code{keep_individuals}, \code{removed}, \code{n_snps_kept}, \code{n_individuals_kept}, \code{call_rate_snp}, \code{call_rate_ind}, \code{maf}, \code{hwe_p}, \code{heterozygosity}, \code{relatedness_matrix}, \code{kinship}, \code{ibd_states}, \code{relatedness}, \code{pruned_snps}, \code{thresholds}, \code{trait}, \code{hwe_test}, \code{note}, \code{method}.
}
\keyword{internal}
\examples{
morie_snpqc1(genotypes = list(a = 1, b = 2))
}
