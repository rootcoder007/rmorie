% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.vepan_variant}
\alias{.vepan_variant}
\title{.vepan_variant}
\usage{
.vepan_variant(v)
}
\arguments{
\item{v}{A list; the body reads \code{$alt}, \code{$chrom}, \code{$id}, \code{$pos}, \code{$ref} from it.}
}
\description{
A step of the vepan_native implementation. Called by \code{morie_vepan_annotate}. See the file header for the source the module follows.
}
\value{
A list with \code{chrom}, \code{pos}, \code{ref}, \code{alt}, \code{id}.
}
\keyword{internal}
