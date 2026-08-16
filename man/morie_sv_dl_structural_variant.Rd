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
\item{pairs}{See Usage.}
\item{reference}{Defaults to \code{NULL}.}
\item{split_reads}{Defaults to \code{NULL}.}
\item{orientation}{Defaults to \code{NULL}.}
\item{median}{Defaults to \code{NULL}.}
\item{sd}{Defaults to \code{NULL}.}
\item{n_sd}{Defaults to \code{3}.}
\item{min_support}{Defaults to \code{2}.}
\item{k}{Defaults to \code{7}.}
\item{k_min}{Defaults to \code{3}.}
\item{min_split_support}{Defaults to \code{2}.}
\item{max_length_diff}{Defaults to \code{0.1}.}
\item{window}{Defaults to \code{NULL}.}
\item{spread}{Defaults to \code{"mad"}.}
}
\description{
Part of the sv_dl_native implementation; see the file header for the source it follows.
}
\value{
A list with \code{estimate}, \code{calls}, \code{n_calls}, \code{n_precise}, \code{insert_median}, \code{insert_sd}, \code{spread}, \code{orientation}, \code{n_sd}, \code{min_support}, \code{method}, \code{note}.
}
\keyword{internal}
