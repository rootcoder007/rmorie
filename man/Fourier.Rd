% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{Fourier}
\alias{Fourier}
\title{Eqs (3.75)-(3.76); the name Section 3.4.4 uses}
\usage{
Fourier(x, t = NULL, omega = NULL, f = NULL, dt = NULL)
}
\arguments{
\item{x}{Passed to \code{Ctft}.}
\item{t}{Passed to \code{Ctft}.}
\item{omega}{Passed to \code{Ctft}.}
\item{f}{Passed to \code{Ctft}.}
\item{dt}{Passed to \code{Ctft}.}
}
\description{
A step of the rangayyan_xfrm implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
The value of \code{Ctft}.
}
\keyword{internal}
\examples{
xs <- c(1, 0.5, -0.25, 0.75, 0)
ts <- c(0, 0.25, 0.5, 0.75, 1)
f0 <- 0.7
Fourier(xs, t = ts, f = f0)
}
