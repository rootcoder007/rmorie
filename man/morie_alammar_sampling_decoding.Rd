% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_alammar_sampling_decoding}
\alias{morie_alammar_sampling_decoding}
\title{morie_alammar_sampling_decoding}
\usage{
morie_alammar_sampling_decoding(logits, seed = 0) 
}
\arguments{
\item{seed}{LCG seed; the Python mirror draws the same tokens.}

\item{logits}{Argument \code{logits}; see Usage.}
}
\description{
morie_alammar_sampling_decoding
}
\value{
A list with \code{tokens}, \code{uniforms}, \code{estimate}, \code{n}, \code{method}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
morie_alammar_sampling_decoding(V)
}
