% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{grad.Fun}
\alias{grad.Fun}
\title{grad.Fun}
\usage{
grad.Fun(self, x)
}
\arguments{
\item{self}{A list; the body reads \code{$.g}, \code{$f} from it.}
\item{x}{Passed to \code{.barerp_num_grad}.}
}
\description{
A step of the barerp_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
The value of \code{.barerp_num_grad}.
}
\keyword{internal}
