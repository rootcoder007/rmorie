% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.pq_encode_i64}
\alias{.pq_encode_i64}
\title{WriteBin has no 64-bit integer; emit two 32-bit halves, folding the}
\usage{
.pq_encode_i64(values)
}
\arguments{
\item{values}{See Usage.}
}
\description{
low half back into signed range because that is all writeBin takes.
}
\value{
The value of \code{writeBin}.
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
