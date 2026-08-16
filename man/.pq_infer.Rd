% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.pq_infer}
\alias{.pq_infer}
\title{Date and POSIXct must keep their logical type on the way out}
\usage{
.pq_infer(v)
}
\arguments{
\item{v}{Numeric; the body checks with \code{is.numeric}.}
}
\description{
Without this a column read as TIMESTAMP_MICROS was written back as a bare DOUBLE of seconds: readable, but no longer a timestamp to any other engine, and off by a factor of 1e6 from where it started.
}
\value{
A list with \code{type}, \code{converted}.
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
