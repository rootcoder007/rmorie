% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.sv_dl_components}
\alias{.sv_dl_components}
\title{Connected components (union-find); singletons dropped}
\usage{
.sv_dl_components(n, edges)
}
\arguments{
\item{n}{A count; the body uses it as \code{seq_len(...)}.}
\item{edges}{A matrix; indexed by row and column.}
}
\description{
A step of the sv_dl_native implementation. Called by \code{morie_sv_dl_paired_end_calls}. See the file header for the source the module follows.
}
\value{
The value of \code{out}, as built in the body.
}
\keyword{internal}
