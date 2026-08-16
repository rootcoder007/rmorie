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
\item{sumstats}{A list; the body reads \code{$beta}, \code{$p}, \code{$position}, \code{$snp} from it.}
\item{ld_ref}{Passed to \code{.prsclm_rows}.}
\item{p_threshold}{Optional; may be \code{NULL}. Coerced to numeric by the body, with \code{as.numeric}.}
\item{r2}{Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{0.1}.}
\item{window}{Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{250000}.}
\item{genotypes}{Optional; may be \code{NULL}. Passed to \code{.prsclm_rows}.}
\item{standardize}{A flag; the body branches on it. Defaults to \code{FALSE}.}
}
\description{
A step of the prsclm_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A list with \code{estimate}, \code{n_retained}, \code{thresholds}, \code{retained}, \code{score}, \code{score_threshold}, \code{scores_by_threshold}, \code{index_variants}, \code{index_variant_names}, \code{clump_of}, \code{clump_members}, \code{clump_sizes}, \code{index_is_most_significant}, \code{n_clumps}, \code{n_variants}, \code{n_individuals}, \code{r2}, \code{window}, \code{standardized}, \code{weights}, \code{method}, \code{note}.
}
\keyword{internal}
