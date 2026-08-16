% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_linwlr_blip}
\alias{morie_linwlr_blip}
\title{morie_linwlr_blip}
\usage{
morie_linwlr_blip(a, w, psi)
}
\arguments{
\item{a}{Passed to \code{.linwlr_vec}.}
\item{w}{Optional; may be \code{NULL}. Passed to \code{.linwlr_mat}.}
\item{psi}{A vector; indexed elementwise.}
}
\description{
A step of the linwlr_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A vector, from \code{as.numeric}.
}
\keyword{internal}
\examples{
morie_linwlr_blip(a = c(1, 2, 3, 4, 5, 6, 7, 8), w = c(1, 2, 3, 4, 5, 6, 7, 8), psi = c(1, 2, 3, 4, 5, 6, 7, 8))
}
