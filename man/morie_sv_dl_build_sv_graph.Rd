% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_sv_dl_build_sv_graph}
\alias{morie_sv_dl_build_sv_graph}
\title{morie_sv_dl_build_sv_graph}
\usage{
morie_sv_dl_build_sv_graph(pairs, median, sd, label, orientation = c("+", "-"), n_sd = 3, window = NULL)
}
\arguments{
\item{pairs}{Iterated over elementwise, with \code{lapply}.}
\item{median}{Numeric; combined arithmetically in the body.}
\item{sd}{Numeric; combined arithmetically in the body.}
\item{label}{A vector; indexed elementwise.}
\item{orientation}{Accepted by the signature and not used anywhere in the body. Defaults to \code{c("+", "-")}.}
\item{n_sd}{Numeric; combined arithmetically in the body. Defaults to \code{3}.}
\item{window}{Optional; may be \code{NULL}. Passed to \code{is.null}.}
}
\description{
A step of the sv_dl_native implementation. Called by \code{morie_sv_dl_paired_end_calls}. See the file header for the source the module follows.
}
\value{
A list with \code{nodes}, \code{edges}, \code{sizes}, \code{label}.
}
\keyword{internal}
