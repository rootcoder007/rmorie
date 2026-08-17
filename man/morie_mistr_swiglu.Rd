% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_mistr_swiglu}
\alias{morie_mistr_swiglu}
\title{SwiGLU gate}
\usage{
morie_mistr_swiglu(x, W1, W2, W3)
}
\arguments{
\item{x}{Input vector.}
\item{W1,W2,W3}{Projection matrices.}
}
\description{
\code{(Swish(x W1) * x W3) W2}.
}
\value{
The gated projection.
}
\keyword{internal}
\examples{
set.seed(1)
x <- rnorm(4)
morie_mistr_swiglu(x, W1 = matrix(rnorm(32, 0, 0.3), 4, 8),
                   W2 = matrix(rnorm(32, 0, 0.3), 8, 4),
                   W3 = matrix(rnorm(32, 0, 0.3), 4, 8))
}
