% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_vepan_pick}
\alias{morie_vepan_pick}
\title{Pick (one record) or --per_gene (one per gene)}
\usage{
morie_vepan_pick(records, per_gene = FALSE)
}
\arguments{
\item{records}{See Usage.}
\item{per_gene}{A flag; the body branches on it. Defaults to \code{FALSE}.}
}
\description{
A step of the vepan_native implementation. Called by \code{morie_vepan_vep_annotation}. See the file header for the source the module follows.
}
\value{
The value of \code{unname}.
}
\keyword{internal}
