% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_alammar_recursive_chunking}
\alias{morie_alammar_recursive_chunking}
\title{Recursive chunking, buffer memory, templates (Alammar Ch 6-12)}
\usage{
morie_alammar_recursive_chunking(text, separators = NULL, target_size = 200, overlap = 0) 
}
\arguments{
\item{text}{Input string.}
\item{separators}{Tier list.}
\item{target_size,overlap}{Sizes.}
}
\description{
Recursive chunking, buffer memory, templates (Alammar Ch 6-12)
}
\value{
A list with \code{chunks}, \code{n_chunks}, \code{max_chunk_length}, \code{overlap}, \code{estimate}, \code{n}, \code{method}.
}
\keyword{internal}
\examples{
morie_alammar_recursive_chunking(text = 5L)
}
