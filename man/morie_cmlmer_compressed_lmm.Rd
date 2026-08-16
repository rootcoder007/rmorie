% Generated from the module source by ledger/gen_rd.R -- roxygen2 hangs
% on this package. Usage comes from args(), so it tracks the formals.
\name{morie_cmlmer_compressed_lmm}
\alias{morie_cmlmer_compressed_lmm}
\alias{morie_cmlmer}
\title{Compressed mixed linear model: average-linkage grouping}
\usage{
morie_cmlmer_compressed_lmm(y, M, K, clusters = NULL, X = NULL, compare_levels = NULL, log_delta_lo = -10, log_delta_hi = 10, max_iter = 200L)
}
\description{
R arm of cmlmer -- compressed mixed linear model: average-linkage grouping on 1 - K, a per-group random effect, the variance ratio profiled out by REML, and a GLS t-test per marker.
}
\value{
A list with components \code{n}, \code{drop}, \code{nrow}, \code{error}, \code{estimate}, \code{beta}, \code{se}, \code{t}, \code{p_value}, \code{group}, \code{n_groups}, \code{group_sizes}, \code{group_kinship}, \code{coefficients}, \code{delta}, \code{sigma2_g}, \code{sigma2_e}, \code{h2}, \code{reml_loglik}, \code{reml_profile}, \code{level_loglik}, \code{n_markers}, \code{p}, \code{clusters_requested}, \code{method}, \code{note}.
}
\references{
Zhang, Z. et al. (2010) Nature Genetics 42(4), 355-360; Yu, J. et al.
(2006) Nature Genetics 38(2), 203-208.
}
\keyword{internal}
