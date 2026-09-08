% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{AncOut}
\alias{AncOut}
\title{Eq. (3.196): e = x - y, and the ERROR is the canceller\'s output}
\usage{
AncOut(x, y) 
}
\arguments{
\item{x}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{y}{Coerced to numeric by the body, with \code{as.numeric}.}
}
\description{
This is the step that surprises: the quantity being minimized is the thing you keep, because minimizing it drives y toward the interference and leaves the signal behind in e.
}
\value{
A list with \code{e}, \code{v_hat}, \code{n}, \code{input_power}, \code{output_power}, \code{power_reduction}, \code{error_is_the_output}, \code{method}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
AncOut(V, V)
}
