% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{hess.Fun}
\alias{hess.Fun}
\title{hess.Fun}
\usage{
hess.Fun(self, x)
}
\arguments{
\item{self}{A list; the body reads \code{$.h}, \code{$affine}, \code{$f} from it.}
\item{x}{A vector; its length is taken.}
}
\description{
A step of the barerp_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
The value of \code{.num_hess}.
}
\keyword{internal}
\examples{
f <- .Fun(function(x) sum(x^2))
hess.Fun(f, c(1, 2))
}
