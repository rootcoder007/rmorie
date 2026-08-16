% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.pq_reader}
\alias{.pq_reader}
\title{A decoder is a plain environment so the position advances by}
\usage{
.pq_reader(buf, pos = 1L)
}
\arguments{
\item{buf}{See Usage.}
\item{pos}{Defaults to \code{1L}.}
}
\description{
reference the way the Python reader\\'s self.pos does; passing it around by value would need every helper to return (value, pos) pairs.
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
