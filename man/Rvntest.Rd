% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{Rvntest}
\alias{Rvntest}
\title{Rank von Neumann test -- eqs. (3.5.1)-(3.5.2), p. 95}
\usage{
Rvntest(x, alternative = "two-sided") 
}
\description{
Rank von Neumann test -- eqs. (3.5.1)-(3.5.2), p. 95
}
\arguments{
\item{x}{Argument \code{x}; see Usage.}
\item{alternative}{Argument \code{alternative}; see Usage.}
}
\value{
A list with \code{statistic}, \code{nm}, \code{denom}, \code{z}, \code{p_value}, \code{mean}, \code{var}, \code{n}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
Rvntest(V)
}
