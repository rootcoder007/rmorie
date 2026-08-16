% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.pq_wfield}
\alias{.pq_wfield}
\title{.pq_wfield}
\usage{
.pq_wfield(e, fid, ttype, last)
}
\arguments{
\item{e}{Passed to \code{.pq_emit}.}
\item{fid}{Numeric; combined arithmetically in the body.}
\item{ttype}{Passed to \code{bitwOr}.}
\item{last}{Numeric; combined arithmetically in the body.}
}
\description{
A step of the parquet implementation. Called by \code{.pq_wbytes}, \code{.pq_wi32}, \code{.pq_wi64} and 4 others in the module. See the file header for the source the module follows.
}
\value{
The value of \code{fid}, as built in the body.
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
.pq_wfield(w, fid = 1L, ttype = 5L, last = 0L)
do.call(c, w$parts)
}
