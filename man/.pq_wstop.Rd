% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.pq_wstop}
\alias{.pq_wstop}
\title{.pq_wstop}
\usage{
.pq_wstop(e)
}
\arguments{
\item{e}{A list; the body reads \code{$parts} from it.}
}
\description{
A step of the parquet implementation. Called by \code{morie_write_parquet}. See the file header for the source the module follows.
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
