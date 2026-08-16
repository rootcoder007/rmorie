% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.pq_scalar}
\alias{.pq_scalar}
\title{.pq_scalar}
\usage{
.pq_scalar(e, ttype)
}
\arguments{
\item{e}{A list; the body reads \code{$pos} from it.}
\item{ttype}{Passed to \code{==}.}
}
\description{
A step of the parquet implementation. Called by \code{.pq_list}, \code{.pq_map}, \code{.pq_struct}. See the file header for the source the module follows.
}
\value{
Nothing; this branch always raises.
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
