% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.vepan_transcript}
\alias{.vepan_transcript}
\title{.vepan_transcript}
\usage{
.vepan_transcript(tr)
}
\arguments{
\item{tr}{A list; the body reads \code{$biotype}, \code{$canonical}, \code{$cds_end}, \code{$cds_start}, \code{$chrom}, \code{$exons}, \code{$gene}, \code{$id}, \code{$strand} from it.}
}
\description{
A step of the vepan_native implementation. Called by \code{morie_vepan_transcript_sequence}. See the file header for the source the module follows.
}
\value{
A list with \code{id}, \code{gene}, \code{chrom}, \code{strand}, \code{exons}, \code{cds_start}, \code{cds_end}, \code{biotype}, \code{canonical}, \code{start}, \code{end}.
}
\keyword{internal}
