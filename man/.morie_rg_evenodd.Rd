% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_rg_evenodd}
\alias{.morie_rg_evenodd}
\title{.morie_rg_evenodd}
\usage{
.morie_rg_evenodd(x, n = NULL)
}
\arguments{
\item{x}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{n}{Optional; may be \code{NULL}. Coerced to integer by the body, with \code{as.integer}.}
}
\description{
A step of the rangayyan_xfrm implementation. Called by \code{EvenOdd}, \code{EvenPart}, \code{OddPart}. See the file header for the source the module follows.
}
\value{
A list with \code{n}, \code{even}, \code{odd}, \code{x}, \code{reconstruction_error}.
}
\keyword{internal}
\examples{
S <- c("a", "b", "c")
.morie_rg_evenodd(S)
}
