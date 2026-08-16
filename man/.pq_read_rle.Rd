% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.pq_read_rle}
\alias{.pq_read_rle}
\title{.pq_read_rle}
\usage{
.pq_read_rle(buf, pos, width, count, end)
}
\arguments{
\item{buf}{A vector; indexed elementwise.}
\item{pos}{Numeric; combined arithmetically in the body.}
\item{width}{A count; the body uses it as \code{seq_len(...)}.}
\item{count}{A count; the body uses it as \code{rep(...)}.}
\item{end}{Passed to \code{<=}.}
}
\description{
A step of the parquet implementation. Called by \code{.pq_column_values}. See the file header for the source the module follows.
}
\value{
A list with \code{values}, \code{pos}.
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
