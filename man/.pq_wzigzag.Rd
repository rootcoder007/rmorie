% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.pq_wzigzag}
\alias{.pq_wzigzag}
\title{.pq_wzigzag}
\usage{
.pq_wzigzag(e, n)
}
\arguments{
\item{e}{Passed to \code{.pq_wvarint}.}
\item{n}{Numeric; combined arithmetically in the body.}
}
\description{
A step of the parquet implementation. Called by \code{.pq_wfield}, \code{.pq_wi32}, \code{.pq_wi64} and 1 others in the module. See the file header for the source the module follows.
}
\value{
The value of \code{.pq_wvarint}.
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
w <- .pq_writer()
.pq_wzigzag(w, -3L)
r <- .pq_reader(do.call(c, w$parts))
.pq_zigzag(r)
}
