% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_vepan_vep_annotation}
\alias{morie_vepan_vep_annotation}
\title{morie_vepan_vep_annotation}
\usage{
morie_vepan_vep_annotation(variants, transcripts, genome, upstream = 5000, downstream = 5000, mode = "all", no_intergenic = FALSE)
}
\arguments{
\item{variants}{See Usage.}
\item{transcripts}{See Usage.}
\item{genome}{See Usage.}
\item{upstream}{Defaults to \code{5000}.}
\item{downstream}{Defaults to \code{5000}.}
\item{mode}{Defaults to \code{"all"}.}
\item{no_intergenic}{Defaults to \code{FALSE}.}
}
\description{
Part of the vepan_native implementation; see the file header for the source it follows.
}
\value{
A list with \code{estimate}, \code{annotations}, \code{n_variants}, \code{n_annotations}, \code{consequence_counts}, \code{mode}, \code{method}, \code{note}.
}
\keyword{internal}
