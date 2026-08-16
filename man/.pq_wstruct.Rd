% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.pq_wstruct}
\alias{.pq_wstruct}
\title{.pq_wstruct}
\usage{
.pq_wstruct(e, fid, body, last)
}
\arguments{
\item{e}{Passed to \code{.pq_wfield}.}
\item{fid}{Passed to \code{.pq_wfield}.}
\item{body}{Passed to \code{.pq_emit}.}
\item{last}{Passed to \code{.pq_wfield}.}
}
\description{
A step of the parquet implementation. Called by \code{morie_write_parquet}. See the file header for the source the module follows.
}
\value{
The value of \code{last}, as built in the body.
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
inner <- .pq_writer()
.pq_wi32(inner, fid = 1L, v = 7L, last = 1L)
.pq_wstruct(w, fid = 2L, body = do.call(c, inner$parts), last = 0L)
do.call(c, w$parts)
}
