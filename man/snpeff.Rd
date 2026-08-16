% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{snpeff}
\alias{snpeff}
\title{snpeff}
\usage{
snpeff(cds, variants, cds_start = 0, upstream = 5000, downstream = 5000, transcript_len = NULL)
}
\arguments{
\item{cds}{See Usage.}
\item{variants}{See Usage.}
\item{cds_start}{Defaults to \code{0}.}
\item{upstream}{Defaults to \code{5000}.}
\item{downstream}{Defaults to \code{5000}.}
\item{transcript_len}{Defaults to \code{NULL}.}
}
\description{
Part of the snpeff_native implementation; see the file header for the source it follows.
}
\value{
A list with \code{estimate}, \code{annotations}, \code{effect_counts}, \code{impact_counts}, \code{n_variants}, \code{protein}, \code{method}, \code{note}.
}
\keyword{internal}
