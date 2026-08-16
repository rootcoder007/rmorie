% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.pq_emit}
\alias{.pq_emit}
\title{.pq_emit}
\usage{
.pq_emit(e, r)
}
\arguments{
\item{e}{A list; the body reads \code{$n}, \code{$parts} from it.}
\item{r}{See Usage.}
}
\description{
A step of the parquet implementation. Called by \code{.pq_wbinary}, \code{.pq_wfield}, \code{.pq_wlisthdr} and 4 others in the module. See the file header for the source the module follows.
}
\value{
Invisibly,nothing; the function is called for its effect.
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
.pq_emit(w, as.raw(c(0x01, 0x02)))
w$buf
}
