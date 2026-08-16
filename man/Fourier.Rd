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
\item{x}{See Usage.}
\item{t}{Defaults to \code{NULL}.}
\item{omega}{Defaults to \code{NULL}.}
\item{f}{Defaults to \code{NULL}.}
\item{dt}{Defaults to \code{NULL}.}
}
\description{
A step of the rangayyan_xfrm implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
The value of \code{Ctft}.
}
\keyword{internal}
