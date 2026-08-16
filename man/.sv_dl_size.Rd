% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.sv_dl_size}
\alias{.sv_dl_size}
\title{The SV size this pair implies, used as the clustering weight}
\usage{
.sv_dl_size(p, label, median)
}
\arguments{
\item{p}{Passed to \code{.sv_dl_insert}.}
\item{label}{A vector; indexed elementwise.}
\item{median}{Numeric; combined arithmetically in the body.}
}
\description{
A step of the sv_dl_native implementation. Called by \code{morie_sv_dl_build_sv_graph}. See the file header for the source the module follows.
}
\value{
A numeric value.
}
\keyword{internal}
