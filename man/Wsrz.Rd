% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{Wsrz}
\alias{Wsrz}
\title{Signed-rank normal approximation -- eq. (5.7.9), p. 202}
\usage{
Wsrz(tplus, n, alternative = "two-sided", correct = FALSE) 
}
\description{
Signed-rank normal approximation -- eq. (5.7.9), p. 202
}
\arguments{
\item{tplus}{Argument \code{tplus}; see Usage.}
\item{n}{Argument \code{n}; see Usage.}
\item{alternative}{Argument \code{alternative}; see Usage.}
\item{correct}{Argument \code{correct}; see Usage.}
}
\value{
A list with \code{z}, \code{p_value}, \code{mean}, \code{var}, \code{statistic}, \code{n}, \code{alternative}.
}
\keyword{internal}
\examples{
rmorie:::Wsrz(tplus = c(1, 2, 3, 4, 5, 6, 7, 8), n = 5L)
}
