% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.vepan_splice_terms}
\alias{.vepan_splice_terms}
\title{Splice consequences from the distance to each exon boundary}
\usage{
.vepan_splice_terms(t, v, lo, hi)
}
\arguments{
\item{t}{A list; the body reads \code{$end}, \code{$exons}, \code{$start}, \code{$strand} from it.}
\item{v}{Accepted by the signature and not used anywhere in the body.}
\item{lo}{Passed to \code{.vepan_seqrange}.}
\item{hi}{Passed to \code{.vepan_seqrange}.}
}
\description{
A step of the vepan_native implementation. Called by \code{morie_vepan_annotate}. See the file header for the source the module follows.
}
\value{
The value of \code{unique}.
}
\keyword{internal}
