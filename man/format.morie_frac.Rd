% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{format.morie_frac}
\alias{format.morie_frac}
\title{format.morie_frac}
\usage{
\method{format}{morie_frac}(x, ...) 
}
\arguments{
\item{x}{A list; the body reads \code{$d}, \code{$n} from it.}
\item{...}{Passed through.}
}
\description{
A step of the rangayyan_class implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A character value.
}
\keyword{internal}
\examples{
D <- data.frame(x = c(1, 2, 3, 4), y = c(2, 4, 5, 9))
rmorie:::format.morie_frac(D)
}
