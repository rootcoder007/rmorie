% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{basic_reproduction_numbers}
\alias{basic_reproduction_numbers}
\title{basic_reproduction_numbers}
\usage{
basic_reproduction_numbers(beta, nu, mu)
}
\arguments{
\item{beta}{See Usage.}
\item{nu}{A vector; its length is taken.}
\item{mu}{See Usage.}
}
\description{
A step of the hiatus_native implementation. Called by \code{.hiatus_simulate}, \code{endemic_equilibrium}. See the file header for the source the module follows.
}
\value{
The value of \code{out}, as built in the body.
}
\keyword{internal}
\examples{
basic_reproduction_numbers(beta = 0.5, nu = 0.5, mu = 5L)
}
