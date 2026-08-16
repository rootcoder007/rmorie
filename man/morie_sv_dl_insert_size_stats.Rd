% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_sv_dl_insert_size_stats}
\alias{morie_sv_dl_insert_size_stats}
\title{morie_sv_dl_insert_size_stats}
\usage{
morie_sv_dl_insert_size_stats(pairs, orientation = NULL, spread = "mad")
}
\arguments{
\item{pairs}{Iterated over elementwise, with \code{lapply}.}
\item{orientation}{Optional; may be \code{NULL}. A vector; indexed elementwise.}
\item{spread}{One of \code{"mad"}, \code{"sd"}. Defaults to \code{"mad"}.}
}
\description{
A step of the sv_dl_native implementation. Called by \code{morie_sv_dl_paired_end_calls}, \code{morie_sv_dl_structural_variant}. See the file header for the source the module follows.
}
\value{
A list with \code{median}, \code{sd}, \code{spread}, \code{orientation}, \code{n}.
}
\keyword{internal}
