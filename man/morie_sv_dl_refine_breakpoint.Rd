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
\item{call}{See Usage.}
\item{reference}{See Usage.}
\item{reads}{See Usage.}
\item{k}{Defaults to \code{7}.}
\item{k_min}{Defaults to \code{3}.}
\item{min_split_support}{Defaults to \code{2}.}
\item{max_length_diff}{Defaults to \code{0.1}.}
\item{match}{Defaults to \code{1}.}
\item{mismatch}{Defaults to \code{-2}.}
\item{gap_open}{Defaults to \code{-4}.}
\item{gap_extend}{Defaults to \code{-1}.}
}
\description{
Part of the sv_dl_native implementation; see the file header for the source it follows.
}
\value{
A list with \code{start}, \code{end}, \code{size}, \code{split_support}, \code{consensus}, \code{score}, \code{microinsertion}, \code{microhomology}, \code{kmer_offset}.
}
\keyword{internal}
