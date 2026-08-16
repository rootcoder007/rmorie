% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.pq_convert}
\alias{.pq_convert}
\title{.pq_convert}
\usage{
.pq_convert(vals, ptype, converted)
}
\arguments{
\item{vals}{Iterated over elementwise, with \code{vapply}.}
\item{ptype}{See Usage.}
\item{converted}{Accepted by the signature and not used anywhere in the body.}
}
\description{
A step of the parquet implementation. Called by \code{morie_read_parquet}. See the file header for the source the module follows.
}
\value{
The value of \code{vals}, as built in the body.
}
\references{
compact protocol spec, and google/snappy/format_description.txt.

ponytail: pure R, no compiled code. The snappy tag loop and the RLE
bit-unpacker are the only hot spots; move those to src/ if a file
large enough to matter ever shows up. Reading the whole 25,000-row
complaints sample is the current worst case and it is seconds, not
minutes.
}
\keyword{internal}
\examples{
.pq_convert(vals = c(1, 2, 3, 4, 5, 6, 7, 8), ptype = 5L, converted = c(1, 2, 3, 4, 5, 6, 7, 8))
}
