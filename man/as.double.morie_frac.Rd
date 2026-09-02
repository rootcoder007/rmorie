% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{as.double.morie_frac}
\alias{as.double.morie_frac}
\title{As.numeric() dispatches to as.double methods, not as.numeric ones}
\usage{
\method{as.double}{morie_frac}(x, ...) 
}
\arguments{
\item{x}{A list; the body reads \code{$d}, \code{$n} from it.}
\item{...}{Passed through.}
}
\description{
A step of the rangayyan_class implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A numeric value.
}
\keyword{internal}
\examples{
D <- data.frame(x = c(1, 2, 3, 4), y = c(2, 4, 5, 9))
as.double.morie_frac(D)
}
