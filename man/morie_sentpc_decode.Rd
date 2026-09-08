% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_sentpc_decode}
\alias{morie_sentpc_decode}
\title{Join pieces and unescape}
\usage{
morie_sentpc_decode(pieces, strip_prefix = TRUE) 
}
\arguments{
\item{pieces}{Character vector of pieces.}
\item{strip_prefix}{Logical; drop a leading U+2581.}
}
\description{
Join pieces and unescape
}
\value{
Character.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
morie_sentpc_decode(V)
}
