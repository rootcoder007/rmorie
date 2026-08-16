% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.linwlr_vec}
\alias{.linwlr_vec}
\title{.linwlr_vec}
\usage{
.linwlr_vec(x)
}
\arguments{
\item{x}{Optional; may be \code{NULL}. Coerced to numeric by the body, with \code{as.numeric}.}
}
\description{
A step of the linwlr_native implementation. Called by \code{morie_linwlr}, \code{morie_linwlr_blip}. See the file header for the source the module follows.
}
\value{
A vector, from \code{as.numeric}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.linwlr_vec(V)
}
