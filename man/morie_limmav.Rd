% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_limmav}
\alias{morie_limmav}
\title{morie_limmav}
\usage{
morie_limmav(counts, design, contrast = NULL, lib_sizes = NULL, span = 0.5, weights = TRUE, moderate = TRUE)
}
\arguments{
\item{counts}{See Usage.}
\item{design}{See Usage.}
\item{contrast}{Defaults to \code{NULL}.}
\item{lib_sizes}{Defaults to \code{NULL}.}
\item{span}{Defaults to \code{0.5}.}
\item{weights}{Defaults to \code{TRUE}.}
\item{moderate}{Defaults to \code{TRUE}.}
}
\description{
Part of the limmav_native implementation; see the file header for the source it follows.
}
\value{
A list with \code{estimate}, \code{log_fold_change}, \code{se}, \code{t}, \code{pvalue}, \code{padj}, \code{df}, \code{df_total}, \code{d0}, \code{s0_sq}, \code{s2_gene}, \code{s2_post}, \code{moderated}, \code{voom_weights}, \code{log_cpm}, \code{mean_log_count}, \code{sqrt_sd}, \code{trend_x}, \code{trend_y}, \code{lib_sizes}, \code{weighted}, \code{n_genes}, \code{n_samples}, \code{note}, \code{method}.
}
\keyword{internal}
