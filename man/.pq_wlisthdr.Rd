% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.pq_wlisthdr}
\alias{.pq_wlisthdr}
\title{.pq_wlisthdr}
\usage{
.pq_wlisthdr(e, size, etype)
}
\arguments{
\item{e}{Passed to \code{.pq_emit}.}
\item{size}{Passed to \code{.pq_wvarint}.}
\item{etype}{Passed to \code{bitwOr}.}
}
\description{
A step of the parquet implementation. Called by \code{.pq_wlistbin}, \code{.pq_wlisti32}, \code{.pq_wliststruct}. See the file header for the source the module follows.
}
\value{
One of two values, depending on the branch taken.
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
