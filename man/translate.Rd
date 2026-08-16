% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{translate}
\alias{translate}
\title{translate}
\usage{
translate(seq, to_stop = FALSE)
}
\arguments{
\item{seq}{Coerced to character by the body, with \code{as.character}.}
\item{to_stop}{A flag; the body branches on it. Defaults to \code{FALSE}.}
}
\description{
A step of the snpeff_native implementation. Called by \code{.vepan_coding_terms}, \code{annotate_variant}, \code{snpeff}. See the file header for the source the module follows.
}
\value{
A character value.
}
\keyword{internal}
\examples{
S <- c("a", "b", "c")
translate(S)
}
