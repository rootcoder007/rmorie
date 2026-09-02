% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{cgmth}
\alias{cgmth}
\title{cgmth}
\usage{
cgmth(f, grad_f, x0, tol = 1e-06, max_iter = 1000L, full_output = FALSE) 
}
\arguments{
\item{f}{Accepted by the signature and not used anywhere in the body.}
\item{grad_f}{Accepted by the signature and not used anywhere in the body.}
\item{x0}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{tol}{Passed to \code{<}. Defaults to \code{1e-06}.}
\item{max_iter}{A count; the body uses it as \code{seq_len(...)}. Defaults to \code{1000L}.}
\item{full_output}{A flag; the body branches on it. Defaults to \code{FALSE}.}
}
\description{
A step of the cgmth_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
The value of \code{x}, as built in the body.
}
\keyword{internal}
\examples{
cgmth(function(x) sum(x^2), function(x) 2 * x, x0 = c(1, 1))
}
