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
