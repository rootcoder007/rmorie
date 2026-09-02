% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_sentpc_escape_whitespace}
\alias{morie_sentpc_escape_whitespace}
\title{Replace spaces with the U+2581 marker}
\usage{
morie_sentpc_escape_whitespace(text, add_prefix = TRUE) 
}
\arguments{
\item{text}{Character.}
\item{add_prefix}{Logical; prepend a marker so leading spaces   survive the round-trip.}
}
\description{
Nothing is dropped, which is the whole point: the transform is invertible by string replacement alone.
}
\value{
Character.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
morie_sentpc_escape_whitespace(V)
}
