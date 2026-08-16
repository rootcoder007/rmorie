% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.mpq_varint}
\alias{.mpq_varint}
\title{.mpq_varint}
\usage{
.mpq_varint(buf, pos)
}
\description{
.mpq_varint
}
\value{
A list with \code{value}, \code{pos}.
}
\keyword{internal}
\examples{
.mpq_varint(buf = c(1, 2, 3, 4, 5, 6, 7, 8), pos = 5L)
}
