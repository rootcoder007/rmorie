% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.linwlr_ridgesolve}
\alias{.linwlr_ridgesolve}
\title{.linwlr_ridgesolve}
\usage{
.linwlr_ridgesolve(M, rhs, ridge)
}
\arguments{
\item{M}{A matrix; passed to \code{nrow}.}
\item{rhs}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{ridge}{Numeric; combined arithmetically in the body.}
}
\description{
A step of the linwlr_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A matrix, from \code{solve}.
}
\keyword{internal}
