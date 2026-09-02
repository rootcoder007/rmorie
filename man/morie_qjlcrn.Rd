% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_qjlcrn}
\alias{morie_qjlcrn}
\title{Random projection with binary coins}
\usage{
morie_qjlcrn(A, k, distribution = "rademacher", seed = 0L) 
}
\arguments{
\item{A}{List of points, each a numeric vector; all rows must   share the same length.}
\item{k}{Target dimension, positive integer.}
\item{distribution}{One of \code{"rademacher"}, \code{"sparse"}.}
\item{seed}{Seed for the shared generator.}
}
\description{
Draws a \code{d}-by-\code{k} matrix \code{R} from the chosen Achlioptas distribution and returns the embedding \code{E = A R / sqrt(k)}, the scaling that preserves the expected squared norm (Achlioptas 2003, Theorem 1.1). As \code{k} grows past the Theorem 1.1 bound \code{k0} the realised pairwise squared distances land in \code{1 +/- epsilon} with probability at least \code{1 - n^-beta} -- this is what \code{distortion} checks on a given \code{E}.
}
\value{
A list with \code{estimate}, \code{embedding} \code{E},   \code{matrix} \code{R}, \code{k}, \code{d}, \code{n},   \code{distribution}, \code{nonzero_fraction}, \code{norm},   \code{method}.
}
\keyword{internal}
\examples{
morie_qjlcrn(A = c(1, 2, 3, 4, 5, 6, 7, 8), k = 5L)
}
