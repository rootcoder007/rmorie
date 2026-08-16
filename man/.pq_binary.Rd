% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.pq_binary}
\alias{.pq_binary}
\title{.pq_binary}
\usage{
.pq_binary(e)
}
\arguments{
\item{e}{A list; the body reads \code{$buf}, \code{$pos} from it.}
}
\description{
A step of the parquet implementation. Called by \code{.pq_scalar}. See the file header for the source the module follows.
}
\value{
The value of \code{out}, as built in the body.
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
w <- .pq_writer()
.pq_wbinary(w, charToRaw("hello"))
r <- .pq_reader(do.call(c, w$parts))
rawToChar(.pq_binary(r))
}
