% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.pq_wbinary}
\alias{.pq_wbinary}
\title{.pq_wbinary}
\usage{
.pq_wbinary(e, b)
}
\arguments{
\item{e}{See Usage.}
\item{b}{See Usage.}
}
\description{
Part of the parquet implementation; see the file header for the source it follows.
}
\value{
The value of \code{.pq_emit}.
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
