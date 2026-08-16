% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.pq_read_footer}
\alias{.pq_read_footer}
\title{.pq_read_footer}
\usage{
.pq_read_footer(con, size)
}
\arguments{
\item{con}{Passed to \code{seek}.}
\item{size}{Numeric; combined arithmetically in the body.}
}
\description{
A step of the parquet implementation. Called by \code{morie_read_parquet}. See the file header for the source the module follows.
}
\value{
The value of \code{.pq_struct}.
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
