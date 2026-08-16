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
\item{pairs}{See Usage.}
\item{median}{See Usage.}
\item{sd}{See Usage.}
\item{label}{See Usage.}
\item{orientation}{Defaults to \code{c("+", "-")}.}
\item{n_sd}{Defaults to \code{3}.}
\item{window}{Defaults to \code{NULL}.}
}
\description{
Part of the sv_dl_native implementation; see the file header for the source it follows.
}
\value{
A list with \code{nodes}, \code{edges}, \code{sizes}, \code{label}.
}
\keyword{internal}
