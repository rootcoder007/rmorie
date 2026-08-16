% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_prsclm_prs_cs_clump}
\alias{morie_prsclm_prs_cs_clump}
\title{morie_prsclm_prs_cs_clump}
\usage{
morie_prsclm_prs_cs_clump(sumstats, ld_ref, p_threshold = NULL, r2 = 0.1, window = 250000, genotypes = NULL, standardize = FALSE)
}
\arguments{
\item{sumstats}{See Usage.}
\item{ld_ref}{See Usage.}
\item{p_threshold}{Defaults to \code{NULL}.}
\item{r2}{Defaults to \code{0.1}.}
\item{window}{Defaults to \code{250000}.}
\item{genotypes}{Defaults to \code{NULL}.}
\item{standardize}{Defaults to \code{FALSE}.}
}
\description{
Part of the prsclm_native implementation; see the file header for the source it follows.
}
\value{
A list with \code{estimate}, \code{n_retained}, \code{thresholds}, \code{retained}, \code{score}, \code{score_threshold}, \code{scores_by_threshold}, \code{index_variants}, \code{index_variant_names}, \code{clump_of}, \code{clump_members}, \code{clump_sizes}, \code{index_is_most_significant}, \code{n_clumps}, \code{n_variants}, \code{n_individuals}, \code{r2}, \code{window}, \code{standardized}, \code{weights}, \code{method}, \code{note}.
}
\keyword{internal}
