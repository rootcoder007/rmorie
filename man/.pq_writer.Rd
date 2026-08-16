% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.pq_writer}
\alias{.pq_writer}
\title{Writer. Accumulates into a list of raw vectors and concatenates once,}
\usage{
.pq_writer()
}
\description{
because growing a raw vector element by element is quadratic.
}
\value{
The value of \code{e}, as built in the body.
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
