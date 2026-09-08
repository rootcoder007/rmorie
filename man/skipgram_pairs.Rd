% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{skipgram_pairs}
\alias{skipgram_pairs}
\title{skipgram_pairs}
\usage{
skipgram_pairs(walks, window = 2) 
}
\arguments{
\item{walks}{See Usage.}
\item{window}{Coerced to integer by the body, with \code{as.integer}. Defaults to \code{2}.}
}
\description{
A step of the node2v_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
The value of \code{do.call}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
skipgram_pairs(V)
}
