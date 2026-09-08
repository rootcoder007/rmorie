% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_burkov_cosine_similarity}
\alias{morie_burkov_cosine_similarity}
\title{Cosine similarity (Burkov Eq 1.5)}
\usage{
morie_burkov_cosine_similarity(x, y) 
}
\arguments{
\item{x,y}{Vectors; neither may be zero.}
}
\description{
Cosine similarity (Burkov Eq 1.5)
}
\value{
A list with \code{estimate}, \code{angle_radians}, \code{n}, \code{method}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
morie_burkov_cosine_similarity(V, V)
}
