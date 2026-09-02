% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{annotate_variant}
\alias{annotate_variant}
\title{annotate_variant}
\usage{
annotate_variant(cds, pos, ref, alt, cds_start = 0, upstream = 5000, downstream = 5000, 
    transcript_len = NULL) 
}
\arguments{
\item{cds}{Coerced to character by the body, with \code{as.character}.}
\item{pos}{Numeric; combined arithmetically in the body.}
\item{ref}{A vector; its length is taken.}
\item{alt}{A vector; its length is taken.}
\item{cds_start}{Numeric; combined arithmetically in the body. Defaults to \code{0}.}
\item{upstream}{Passed to \code{<=}. Defaults to \code{5000}.}
\item{downstream}{Passed to \code{<=}. Defaults to \code{5000}.}
\item{transcript_len}{Optional; may be \code{NULL}. Coerced to integer by the body, with \code{as.integer}.}
}
\description{
A step of the snpeff_native implementation. Called by \code{snpeff}. See the file header for the source the module follows.
}
\value{
The value of \code{.snpeff_pack}.
}
\keyword{internal}
\examples{
annotate_variant("ATGAAACCCGGGTTTTAA", 4, "A", "T")
}
