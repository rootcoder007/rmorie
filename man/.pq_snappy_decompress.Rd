% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.pq_snappy_decompress}
\alias{.pq_snappy_decompress}
\title{.pq_snappy_decompress}
\usage{
.pq_snappy_decompress(data)
}
\arguments{
\item{data}{A vector; its length is taken and its elements indexed.}
}
\description{
A step of the parquet implementation. Called by \code{.pq_column_values}. See the file header for the source the module follows.
}
\value{
The value of \code{out}, as built in the body.
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
