% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_rappor_bloom}
\alias{.morie_rappor_bloom}
\title{.morie_rappor_bloom}
\usage{
.morie_rappor_bloom(value, k, h, cohort = 0)
}
\arguments{
\item{value}{Coerced to character by the body, with \code{as.character}.}
\item{k}{Numeric; combined arithmetically in the body.}
\item{h}{A count; the body uses it as \code{seq_len(...)}.}
\item{cohort}{Numeric; combined arithmetically in the body. Defaults to \code{0}.}
}
\description{
A step of the rappor_native implementation. Called by \code{morie_rappor}. See the file header for the source the module follows.
}
\value{
A vector, from \code{sort}.
}
\keyword{internal}
\examples{
.morie_rappor_bloom(value = c(1, 2, 3, 4, 5, 6, 7, 8), k = 5L, h = 0.5)
}
