% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{derivatives}
\alias{derivatives}
\title{derivatives}
\usage{
derivatives(S, I, beta, nu, mu, sigma) 
}
\arguments{
\item{S}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{I}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{beta}{Passed to \code{._check}.}
\item{nu}{Passed to \code{._check}.}
\item{mu}{Passed to \code{._check}.}
\item{sigma}{Passed to \code{._check}.}
}
\description{
A step of the hiatus_native implementation. Called by \code{.hiatus_simulate}. See the file header for the source the module follows.
}
\value{
A list with \code{dS}, \code{dI}.
}
\keyword{internal}
