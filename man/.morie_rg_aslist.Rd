% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_rg_aslist}
\alias{.morie_rg_aslist}
\title{.morie_rg_aslist}
\usage{
.morie_rg_aslist(x)
}
\arguments{
\item{x}{Optional; may be \code{NULL}. Coerced to numeric by the body, with \code{as.numeric}.}
}
\description{
A step of the rangayyan_ch3 implementation. Called by \code{ContConv}, \code{CovXY}, \code{DeltaDecomp} and 10 others in the module. See the file header for the source the module follows.
}
\value{
A vector, from \code{as.numeric}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.morie_rg_aslist(V)
}
