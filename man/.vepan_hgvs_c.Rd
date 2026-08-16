% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.vepan_hgvs_c}
\alias{.vepan_hgvs_c}
\title{C. notation, with an indel shifted to its most 3\\' position}
\usage{
.vepan_hgvs_c(t, v, seq, gpos, cds_first)
}
\arguments{
\item{t}{A list; the body reads \code{$strand} from it.}
\item{v}{A list; the body reads \code{$alt}, \code{$pos} from it.}
\item{seq}{A vector; its length is taken.}
\item{gpos}{A vector; its length is taken.}
\item{cds_first}{Optional; may be \code{NULL}. Numeric; combined arithmetically in the body.}
}
\description{
A step of the vepan_native implementation. Called by \code{morie_vepan_annotate}. See the file header for the source the module follows.
}
\value{
A character value.
}
\keyword{internal}
