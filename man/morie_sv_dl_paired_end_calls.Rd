% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_sv_dl_paired_end_calls}
\alias{morie_sv_dl_paired_end_calls}
\title{morie_sv_dl_paired_end_calls}
\usage{
morie_sv_dl_paired_end_calls(pairs, median = NULL, sd = NULL, orientation = NULL, n_sd = 3, min_support = 2, window = NULL, spread = "mad")
}
\arguments{
\item{pairs}{Iterated over elementwise, with \code{lapply}.}
\item{median}{Optional; may be \code{NULL}. Passed to \code{morie_sv_dl_classify_pair}.}
\item{sd}{Optional; may be \code{NULL}. Passed to \code{morie_sv_dl_classify_pair}.}
\item{orientation}{Optional; may be \code{NULL}. Passed to \code{morie_sv_dl_insert_size_stats}.}
\item{n_sd}{Passed to \code{morie_sv_dl_classify_pair}. Defaults to \code{3}.}
\item{min_support}{Defaults to \code{2}.}
\item{window}{Passed to \code{morie_sv_dl_build_sv_graph}.}
\item{spread}{Passed to \code{morie_sv_dl_insert_size_stats}. Defaults to \code{"mad"}.}
}
\description{
A step of the sv_dl_native implementation. Called by \code{morie_sv_dl_structural_variant}. See the file header for the source the module follows.
}
\value{
The value of \code{calls}, as built in the body.
}
\keyword{internal}
