% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{Normscores}
\alias{Normscores}
\title{Terry-Hoeffding normal-scores test -- Sec. 8.3.1, p. 299}
\usage{
Normscores(x, y, nodes = 4001) 
}
\description{
Terry-Hoeffding normal-scores test -- Sec. 8.3.1, p. 299
}
\arguments{
\item{x}{Argument \code{x}; see Usage.}
\item{y}{Argument \code{y}; see Usage.}
\item{nodes}{Argument \code{nodes}; see Usage.}
}
\value{
A list with \code{statistic}, \code{z}, \code{p_value}, \code{mean}, \code{var}, \code{scores}, \code{m}, \code{n}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
Normscores(V, V)
}
