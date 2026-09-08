% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_perK}
\alias{morie_perK}
\title{morie_perK}
\usage{
morie_perK(x1, x2 = NULL, period = 1, lengthscale = 1, variance = 1) 
}
\arguments{
\item{x1}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{x2}{Optional; may be \code{NULL}. Coerced to numeric by the body, with \code{as.numeric}.}
\item{period}{Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{1}.}
\item{lengthscale}{Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{1}.}
\item{variance}{Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{1}.}
}
\description{
A step of the perK_mixedcase_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A list with \code{K}, \code{shape}, \code{period}, \code{lengthscale}, \code{variance}, \code{diag_is_variance}, \code{method}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
morie_perK(V)
}
