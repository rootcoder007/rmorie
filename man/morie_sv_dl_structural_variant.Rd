% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_sv_dl_structural_variant}
\alias{morie_sv_dl_structural_variant}
\title{morie_sv_dl_structural_variant}
\usage{
morie_sv_dl_structural_variant(pairs, reference = NULL, split_reads = NULL, orientation = NULL, median = NULL, sd = NULL, n_sd = 3, min_support = 2, k = 7, k_min = 3, min_split_support = 2, max_length_diff = 0.1, window = NULL, spread = "mad")
}
\arguments{
\item{pairs}{Passed to \code{morie_sv_dl_paired_end_calls}.}
\item{reference}{Optional; may be \code{NULL}. A vector; its length is taken.}
\item{split_reads}{Optional; may be \code{NULL}. A vector; its length is taken.}
\item{orientation}{Passed to \code{morie_sv_dl_paired_end_calls}.}
\item{median}{Passed to \code{morie_sv_dl_paired_end_calls}.}
\item{sd}{Passed to \code{morie_sv_dl_paired_end_calls}.}
\item{n_sd}{Passed to \code{morie_sv_dl_paired_end_calls}. Defaults to \code{3}.}
\item{min_support}{Passed to \code{morie_sv_dl_paired_end_calls}. Defaults to \code{2}.}
\item{k}{Passed to \code{morie_sv_dl_refine_breakpoint}. Defaults to \code{7}.}
\item{k_min}{Passed to \code{morie_sv_dl_refine_breakpoint}. Defaults to \code{3}.}
\item{min_split_support}{Passed to \code{morie_sv_dl_refine_breakpoint}. Defaults to \code{2}.}
\item{max_length_diff}{Passed to \code{morie_sv_dl_refine_breakpoint}. Defaults to \code{0.1}.}
\item{window}{Passed to \code{morie_sv_dl_paired_end_calls}.}
\item{spread}{Passed to \code{morie_sv_dl_paired_end_calls}. Defaults to \code{"mad"}.}
}
\description{
A step of the sv_dl_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A list with \code{estimate}, \code{calls}, \code{n_calls}, \code{n_precise}, \code{insert_median}, \code{insert_sd}, \code{spread}, \code{orientation}, \code{n_sd}, \code{min_support}, \code{method}, \code{note}.
}
\keyword{internal}
