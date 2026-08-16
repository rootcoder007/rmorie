% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.pq_column_values}
\alias{.pq_column_values}
\title{.pq_column_values}
\usage{
.pq_column_values(con, cm, num_rows, maxdef, typelen)
}
\arguments{
\item{con}{Passed to \code{seek}.}
\item{cm}{Passed to \code{.pq_f}.}
\item{num_rows}{A count; the body uses it as \code{seq_len(...)}.}
\item{maxdef}{Passed to \code{.pq_bit_width}.}
\item{typelen}{Passed to \code{.pq_decode_plain}.}
}
\description{
A step of the parquet implementation. Called by \code{morie_read_parquet}. See the file header for the source the module follows.
}
\value{
The value of \code{[}.
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
