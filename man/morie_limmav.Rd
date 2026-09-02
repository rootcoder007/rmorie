% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_limmav}
\alias{morie_limmav}
\title{morie_limmav}
\usage{
morie_limmav(counts, design, contrast = NULL, lib_sizes = NULL, span = 0.5, 
    weights = TRUE, moderate = TRUE) 
}
\arguments{
\item{counts}{Passed to \code{.limmav_voom_weights}.}
\item{design}{A vector; its length is taken and its elements indexed.}
\item{contrast}{Optional; may be \code{NULL}. A vector; its length is taken.}
\item{lib_sizes}{Passed to \code{.limmav_voom_weights}.}
\item{span}{Passed to \code{.limmav_voom_weights}. Defaults to \code{0.5}.}
\item{weights}{A flag; the body branches on it. Defaults to \code{TRUE}.}
\item{moderate}{A flag; the body branches on it. Defaults to \code{TRUE}.}
}
\description{
A step of the limmav_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A list with \code{estimate}, \code{log_fold_change}, \code{se}, \code{t}, \code{pvalue}, \code{padj}, \code{df}, \code{df_total}, \code{d0}, \code{s0_sq}, \code{s2_gene}, \code{s2_post}, \code{moderated}, \code{voom_weights}, \code{log_cpm}, \code{mean_log_count}, \code{sqrt_sd}, \code{trend_x}, \code{trend_y}, \code{lib_sizes}, \code{weighted}, \code{n_genes}, \code{n_samples}, \code{note}, \code{method}.
}
\keyword{internal}
