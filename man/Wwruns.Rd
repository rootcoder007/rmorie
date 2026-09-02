% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{Wwruns}
\alias{Wwruns}
\title{Wald-Wolfowitz two-sample runs test -- Sec. 6.2, p. 231}
\usage{
Wwruns(x, y) 
}
\description{
Wald-Wolfowitz two-sample runs test -- Sec. 6.2, p. 231
}
\arguments{
\item{x}{Argument \code{x}; see Usage.}
\item{y}{Argument \code{y}; see Usage.}
}
\value{
A list with \code{statistic}, \code{p_value}, \code{z}, \code{p_normal}, \code{mean}, \code{var}, \code{m}, \code{n}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
Wwruns(V, V)
}
