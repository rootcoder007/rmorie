% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{MfInput}
\alias{MfInput}
\title{Eq (4.33): X(omega) = integral x(t) exp(-j omega t) dt.  An integral,}
\usage{
MfInput(x, omega, dt = 1)
}
\arguments{
\item{x}{See Usage.}
\item{omega}{See Usage.}
\item{dt}{Defaults to \code{1}.}
}
\description{
so it carries the sampling interval.
}
\value{
A list with \code{X}, \code{omega}, \code{dt}, \code{n}, \code{method}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
MfInput(V, V)
}
