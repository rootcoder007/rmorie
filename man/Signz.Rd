% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{Signz}
\alias{Signz}
\title{Sign-test normal approximation -- eq. (5.4.7), p. 174}
\usage{
Signz(k, n, alternative = "two-sided", correct = TRUE) 
}
\description{
Sign-test normal approximation -- eq. (5.4.7), p. 174
}
\arguments{
\item{k}{Argument \code{k}; see Usage.}
\item{n}{Argument \code{n}; see Usage.}
\item{alternative}{Argument \code{alternative}; see Usage.}
\item{correct}{Argument \code{correct}; see Usage.}
}
\value{
A list with \code{z}, \code{p_value}, \code{statistic}, \code{n}, \code{mean}, \code{var}, \code{alternative}.
}
\keyword{internal}
\examples{
rmorie:::Signz(k = 5L, n = 5L)
}
