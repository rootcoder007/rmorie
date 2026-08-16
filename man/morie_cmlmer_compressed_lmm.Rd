% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_cmlmer_compressed_lmm}
\alias{morie_cmlmer_compressed_lmm}
\title{morie_cmlmer_compressed_lmm}
\usage{
morie_cmlmer_compressed_lmm(y, M, K, clusters = NULL, X = NULL, compare_levels = NULL, log_delta_lo = -10, log_delta_hi = 10, max_iter = 200L)
}
\arguments{
\item{y}{See Usage.}
\item{M}{See Usage.}
\item{K}{See Usage.}
\item{clusters}{Defaults to \code{NULL}.}
\item{X}{Defaults to \code{NULL}.}
\item{compare_levels}{Defaults to \code{NULL}.}
\item{log_delta_lo}{Defaults to \code{-10}.}
\item{log_delta_hi}{Defaults to \code{10}.}
\item{max_iter}{Defaults to \code{200L}.}
}
\description{
Part of the cmlmer_native implementation; see the file header for the source it follows.
}
\value{
A list with \code{estimate}, \code{beta}, \code{se}, \code{t}, \code{p_value}, \code{group}, \code{n_groups}, \code{group_sizes}, \code{group_kinship}, \code{coefficients}, \code{delta}, \code{sigma2_g}, \code{sigma2_e}, \code{h2}, \code{reml_loglik}, \code{reml_profile}, \code{level_loglik}, \code{n}, \code{n_markers}, \code{p}, \code{clusters_requested}, \code{method}, \code{note}.
}
\keyword{internal}
