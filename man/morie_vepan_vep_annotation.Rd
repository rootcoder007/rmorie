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
\item{variants}{Iterated over elementwise, with \code{lapply}.}
\item{transcripts}{See Usage.}
\item{genome}{Passed to \code{morie_vepan_annotate}.}
\item{upstream}{Passed to \code{morie_vepan_annotate}. Defaults to \code{5000}.}
\item{downstream}{Passed to \code{morie_vepan_annotate}. Defaults to \code{5000}.}
\item{mode}{One of \code{"all"}, \code{"per_gene"}, \code{"pick"}. Defaults to \code{"all"}.}
\item{no_intergenic}{A flag; the body branches on it. Defaults to \code{FALSE}.}
}
\description{
A step of the vepan_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A list with \code{estimate}, \code{annotations}, \code{n_variants}, \code{n_annotations}, \code{consequence_counts}, \code{mode}, \code{method}, \code{note}.
}
\keyword{internal}
\examples{
genome <- "AATGGCCTAGGCTTAA"
tr <- list(id = "tx1", gene = "g1", chrom = "chr1",
           exons = rbind(c(3L, 8L), c(11L, 14L)), strand = "+",
           cds = c(3L, 14L))
v <- list(chrom = "chr1", pos = 5L, ref = "G", alt = "A")
r <- morie_vepan_vep_annotation(list(v), list(tr), genome)
str(r, max.level = 1)
}
