% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{snpeff}
\alias{snpeff}
\title{snpeff}
\usage{
snpeff(cds, variants, cds_start = 0, upstream = 5000, downstream = 5000, 
    transcript_len = NULL) 
}
\arguments{
\item{cds}{Coerced to character by the body, with \code{as.character}.}
\item{variants}{A vector; its length is taken and its elements indexed.}
\item{cds_start}{Numeric; combined arithmetically in the body. Defaults to \code{0}.}
\item{upstream}{Passed to \code{annotate_variant}. Defaults to \code{5000}.}
\item{downstream}{Passed to \code{annotate_variant}. Defaults to \code{5000}.}
\item{transcript_len}{Optional; may be \code{NULL}. Coerced to integer by the body, with \code{as.integer}.}
}
\description{
A step of the snpeff_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A list with \code{estimate}, \code{annotations}, \code{effect_counts}, \code{impact_counts}, \code{n_variants}, \code{protein}, \code{method}, \code{note}.
}
\keyword{internal}
