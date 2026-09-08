% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{Ctrlmed}
\alias{Ctrlmed}
\title{Control median test -- Sec. 6.5, eq. (6.5.1), p. 256}
\usage{
Ctrlmed(x, y, alternative = "two-sided") 
}
\description{
Control median test -- Sec. 6.5, eq. (6.5.1), p. 256
}
\arguments{
\item{x}{Argument \code{x}; see Usage.}
\item{y}{Argument \code{y}; see Usage.}
\item{alternative}{Argument \code{alternative}; see Usage.}
}
\value{
A list with \code{statistic}, \code{p_value}, \code{z}, \code{mean}, \code{var}, \code{pmf}, \code{r}, \code{m}, \code{n}.
}
\keyword{internal}
\examples{
rmorie:::Ctrlmed(x = c(1, 2, 3, 4, 5, 6, 7, 8), y = 5L)
}
