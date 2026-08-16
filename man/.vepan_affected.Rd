% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.vepan_affected}
\alias{.vepan_affected}
\title{Genomic bases the variant changes (1-based, inclusive). Insertion}
\usage{
.vepan_affected(v)
}
\arguments{
\item{v}{A list; the body reads \code{$pos}, \code{$ref} from it.}
}
\description{
yields lo > hi (an empty span between pos and pos+1).
}
\value{
A vector, from \code{c}.
}
\keyword{internal}
