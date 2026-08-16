% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{print.andmnh}
\alias{print.andmnh}
\title{print.andmnh}
\usage{
print.andmnh(x, ...)
}
\arguments{
\item{x}{A list; the body reads \code{$alpha}, \code{$bandwidth}, \code{$bandwidth_automatic}, \code{$J}, \code{$kernel}, \code{$n}, \code{$n_params}, \code{$p}, \code{$prewhitened}, \code{$var_order} from it.}
\item{...}{Passed through.}
}
\description{
A step of the andmnh_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
Invisibly,the value of \code{x}, as built in the body.
}
\keyword{internal}
\examples{
D <- data.frame(x = c(1, 2, 3, 4), y = c(2, 4, 5, 9))
print.andmnh(D)
}
