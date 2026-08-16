% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.ghc_deseq2_maximise_log_alpha}
\alias{.ghc_deseq2_maximise_log_alpha}
\title{.ghc_deseq2_maximise_log_alpha}
\usage{
.ghc_deseq2_maximise_log_alpha(obj, lo = -15, hi = 5, n_grid = 60L, refine = 60L)
}
\arguments{
\item{obj}{See Usage.}
\item{lo}{Numeric; passed to \code{exp}. Defaults to \code{-15}.}
\item{hi}{Numeric; combined arithmetically in the body. Defaults to \code{5}.}
\item{n_grid}{A count; the body uses it as \code{seq_len(...)}. Defaults to \code{60L}.}
\item{refine}{A count; the body uses it as \code{seq_len(...)}. Defaults to \code{60L}.}
}
\description{
A step of the deseq2_native implementation. Called by \code{deseq2}, \code{dispersion_gene_wise}. See the file header for the source the module follows.
}
\value{
A numeric value.
}
\keyword{internal}
