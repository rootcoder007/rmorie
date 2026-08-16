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
\item{con}{See Usage.}
\item{cm}{See Usage.}
\item{num_rows}{See Usage.}
\item{maxdef}{See Usage.}
\item{typelen}{See Usage.}
}
\description{
Part of the parquet implementation; see the file header for the source it follows.
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
