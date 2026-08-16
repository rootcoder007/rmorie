% Generated from the module source by ledger/gen_rd.R -- roxygen2 hangs
% on this package. Usage comes from args(), so it tracks the formals.
\name{morie_prsclm_prs_cs_clump}
\alias{morie_prsclm_prs_cs_clump}
\alias{morie_prsclm}
\title{PLINK-style LD clumping followed by p-value}
\usage{
morie_prsclm_prs_cs_clump(sumstats, ld_ref, p_threshold = NULL, r2 = 0.1, window = 250000, genotypes = NULL, standardize = FALSE)
}
\description{
R arm of prsclm -- PLINK-style LD clumping followed by p-value thresholding into polygenic scores.
}
\value{
A list with components \code{m}, \code{j}, \code{drop}, \code{estimate}, \code{n_retained}, \code{thresholds}, \code{retained}, \code{score}, \code{score_threshold}, \code{scores_by_threshold}, \code{index_variants}, \code{index_variant_names}, \code{clump_of}, \code{clump_members}, \code{clump_sizes}, \code{index_is_most_significant}, \code{n_clumps}, \code{n_variants}, \code{n_individuals}, \code{r2}, \code{window}, \code{standardized}, \code{weights}, \code{method}, \code{note}.
}
\references{
Purcell, S. et al. (2007) Am. J. Hum. Genet. 81(3), 559-575;
International Schizophrenia Consortium (2009) Nature 460, 748-752;
Choi, S. W. & O'Reilly, P. F. (2019) GigaScience 8(7), giz082.
}
\keyword{internal}
