% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.pq_snappy_compress}
\alias{.pq_snappy_compress}
\title{Literal-only stream: fully conformant, just does not shrink}
\usage{
.pq_snappy_compress(data)
}
\arguments{
\item{data}{A vector; its length is taken and its elements indexed.}
}
\description{
ponytail: no match-finder; add one if written size ever matters.
}
\value{
The value of \code{do.call}.
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
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.pq_snappy_compress(V)
}
