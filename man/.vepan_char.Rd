% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.vepan_char}
\alias{.vepan_char}
\title{0-based character access}
\usage{
.vepan_char(s, k)
}
\arguments{
\item{s}{Character; passed to \code{substr}.}
\item{k}{Numeric; combined arithmetically in the body.}
}
\description{
A step of the vepan_native implementation. Called by \code{.vepan_cds_frame}, \code{.vepan_coding_terms}, \code{.vepan_hgvs_c}. See the file header for the source the module follows.
}
\value{
The value of \code{substr}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.vepan_char(V, V)
}
