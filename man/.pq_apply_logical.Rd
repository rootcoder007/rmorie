% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.pq_apply_logical}
\alias{.pq_apply_logical}
\title{Logical (converted) types. Applied identically in the Python arm, so}
\usage{
.pq_apply_logical(v, ptype, converted)
}
\arguments{
\item{v}{See Usage.}
\item{ptype}{See Usage.}
\item{converted}{See Usage.}
}
\description{
the two agree on the epoch value; without this a DATE column came back as a bare day count while nanoparquet handed the caller a Date, which is a silent factor-of-86400 trap for anyone swapping the engines.
}
\value{
The value of \code{v}, as built in the body.
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
