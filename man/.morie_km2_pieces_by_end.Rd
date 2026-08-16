% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_km2_pieces_by_end}
\alias{.morie_km2_pieces_by_end}
\title{.morie_km2_pieces_by_end}
\usage{
.morie_km2_pieces_by_end(text, vocab, maxlen)
}
\arguments{
\item{text}{A vector; its length is taken.}
\item{vocab}{A vector; indexed elementwise.}
\item{maxlen}{Numeric; passed to \code{min}.}
}
\description{
A step of the kamath_llm2_native implementation. Called by \code{.morie_km2_forward}. See the file header for the source the module follows.
}
\value{
The value of \code{ends}, as built in the body.
}
\keyword{internal}
\examples{
.morie_km2_pieces_by_end(text = 5L, vocab = data.frame(x = c(1, 2, 3, 4), y = c(2, 4, 5, 9)), maxlen = c(1, 2, 3, 4, 5, 6, 7, 8))
}
