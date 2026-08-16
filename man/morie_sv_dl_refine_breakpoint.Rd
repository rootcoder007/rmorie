% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_sv_dl_refine_breakpoint}
\alias{morie_sv_dl_refine_breakpoint}
\title{morie_sv_dl_refine_breakpoint}
\usage{
morie_sv_dl_refine_breakpoint(call, reference, reads, k = 7, k_min = 3, min_split_support = 2, max_length_diff = 0.1, match = 1, mismatch = -2, gap_open = -4, gap_extend = -1)
}
\arguments{
\item{call}{A list; the body reads \code{$size}, \code{$type} from it.}
\item{reference}{Passed to \code{morie_sv_dl_deletion_type_reference}.}
\item{reads}{A vector; its length is taken and its elements indexed.}
\item{k}{Passed to \code{morie_sv_dl_kmer_diagonals}. Defaults to \code{7}.}
\item{k_min}{Passed to \code{morie_sv_dl_kmer_diagonals}. Defaults to \code{3}.}
\item{min_split_support}{Defaults to \code{2}.}
\item{max_length_diff}{Numeric; combined arithmetically in the body. Defaults to \code{0.1}.}
\item{match}{Passed to \code{morie_sv_dl_gotoh_score_vectors}. Defaults to \code{1}.}
\item{mismatch}{Passed to \code{morie_sv_dl_gotoh_score_vectors}. Defaults to \code{-2}.}
\item{gap_open}{Passed to \code{morie_sv_dl_gotoh_score_vectors}. Defaults to \code{-4}.}
\item{gap_extend}{Passed to \code{morie_sv_dl_gotoh_score_vectors}. Defaults to \code{-1}.}
}
\description{
A step of the sv_dl_native implementation. Called by \code{morie_sv_dl_structural_variant}. See the file header for the source the module follows.
}
\value{
A list with \code{start}, \code{end}, \code{size}, \code{split_support}, \code{consensus}, \code{score}, \code{microinsertion}, \code{microhomology}, \code{kmer_offset}.
}
\keyword{internal}
