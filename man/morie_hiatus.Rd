% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_hiatus}
\alias{morie_hiatus}
\title{morie_hiatus}
\usage{
morie_hiatus(beta, nu, mu, sigma, S0 = NULL, I0 = NULL, t_end = 2000, dt = 0.05, mutation = 0, record_every = 100L)
}
\arguments{
\item{beta}{Passed to \code{.hiatus_simulate}.}
\item{nu}{Passed to \code{.hiatus_simulate}.}
\item{mu}{Passed to \code{.hiatus_simulate}.}
\item{sigma}{Passed to \code{.hiatus_simulate}.}
\item{S0}{Passed to \code{.hiatus_simulate}.}
\item{I0}{Passed to \code{.hiatus_simulate}.}
\item{t_end}{Passed to \code{.hiatus_simulate}. Defaults to \code{2000}.}
\item{dt}{Passed to \code{.hiatus_simulate}. Defaults to \code{0.05}.}
\item{mutation}{Passed to \code{.hiatus_simulate}. Defaults to \code{0}.}
\item{record_every}{Passed to \code{.hiatus_simulate}. Defaults to \code{100L}.}
}
\description{
A step of the hiatus_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
The value of \code{.hiatus_simulate}.
}
\keyword{internal}
\examples{
morie_hiatus(beta = 0.5, nu = 0.5, mu = 5L, sigma = 0.5)
}
