% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.vepan_apply}
\alias{.vepan_apply}
\title{The coding sequence after the variant, and where it changed}
\usage{
.vepan_apply(cds, v, coding, gpos, strand)
}
\arguments{
\item{cds}{See Usage.}
\item{v}{See Usage.}
\item{coding}{See Usage.}
\item{gpos}{See Usage.}
\item{strand}{See Usage.}
}
\description{
(0-based offset). Returns list(alt_cds, off) or list(NULL, NULL).
}
\value{
A list with \code{alt_cds}, \code{off}.
}
\keyword{internal}
