% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.pq_decode_plain}
\alias{.pq_decode_plain}
\title{.pq_decode_plain}
\usage{
.pq_decode_plain(buf, pos, ptype, count, type_length = NULL)
}
\arguments{
\item{buf}{See Usage.}
\item{pos}{See Usage.}
\item{ptype}{See Usage.}
\item{count}{See Usage.}
\item{type_length}{Defaults to \code{NULL}.}
}
\description{
Part of the parquet implementation; see the file header for the source it follows.
}
\value{
Nothing; this branch always raises.
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
