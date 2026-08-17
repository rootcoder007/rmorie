% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_vepan_annotate}
\alias{morie_vepan_annotate}
\title{morie_vepan_annotate}
\usage{
morie_vepan_annotate(variant, transcripts, genome, upstream = 5000, downstream = 5000)
}
\arguments{
\item{variant}{Passed to \code{.vepan_variant}.}
\item{transcripts}{Iterated over elementwise, with \code{lapply}.}
\item{genome}{Coerced to character by the body, with \code{as.character}.}
\item{upstream}{Passed to \code{<}. Defaults to \code{5000}.}
\item{downstream}{Passed to \code{<}. Defaults to \code{5000}.}
}
\description{
A step of the vepan_native implementation. Called by \code{morie_vepan_vep_annotation}. See the file header for the source the module follows.
}
\value{
The value of \code{[}.
}
\keyword{internal}
\examples{
genome <- "AATGGCCTAGGCTTAA"
tr <- list(id = "tx1", gene = "g1", chrom = "chr1",
           exons = rbind(c(3L, 8L), c(11L, 14L)), strand = "+",
           cds = c(3L, 14L))
v <- list(chrom = "chr1", pos = 5L, ref = "G", alt = "A")
r <- morie_vepan_annotate(v, list(tr), genome)
str(r, max.level = 1)
}
