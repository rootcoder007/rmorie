% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.pq_u32}
\alias{.pq_u32}
\title{ReadBin ignores signed = FALSE for sizes above 2 and warns about it,}
\usage{
.pq_u32(r)
}
\arguments{
\item{r}{See Usage.}
}
\description{
so unsigned 32-bit values (byte-array lengths, the footer length, the low half of an INT64) have to be read signed and folded back up.
}
\value{
The value of \code{ifelse}.
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
