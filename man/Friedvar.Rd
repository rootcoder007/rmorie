% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{Friedvar}
\alias{Friedvar}
\title{Friedman S and Q moments -- eq. (12.2.7), p. 442}
\usage{
Friedvar(k, n) 
}
\description{
Friedman S and Q moments -- eq. (12.2.7), p. 442
}
\arguments{
\item{k}{Argument \code{k}; see Usage.}
\item{n}{Argument \code{n}; see Usage.}
}
\value{
A list with \code{mean_s}, \code{var_s}, \code{mean_q}, \code{var_q}, \code{var_chi2}, \code{deficit}, \code{k}, \code{n}.
}
\keyword{internal}
\examples{
Friedvar(k = 5L, n = 5L)
}
