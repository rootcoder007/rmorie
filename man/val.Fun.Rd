% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{val.Fun}
\alias{val.Fun}
\title{val.Fun}
\usage{
val.Fun(self, x) 
}
\arguments{
\item{self}{A list; the body reads \code{$f} from it.}
\item{x}{See Usage.}
}
\description{
A step of the barerp_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A vector, from \code{as.numeric}.
}
\keyword{internal}
\examples{
f <- .Fun(function(x) sum(x^2))
val.Fun(f, c(1, 2))
}
