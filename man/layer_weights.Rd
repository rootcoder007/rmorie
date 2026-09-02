% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{layer_weights}
\alias{layer_weights}
\title{Softmax-normalised s^\{task\}. A simplex, not free weights: these}
\usage{
layer_weights(raw) 
}
\arguments{
\item{raw}{A vector; its length is taken.}
}
\description{
choose WHICH layers to read and cannot alter the magnitude, which is gamma\'s job alone.
}
\value{
A numeric value.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
layer_weights(V)
}
