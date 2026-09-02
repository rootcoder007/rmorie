% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{Moodscale}
\alias{Moodscale}
\title{Mood scale test -- eqs. (9.2.1)-(9.2.3), pp. 314-316}
\usage{
Moodscale(x, y) 
}
\description{
Mood scale test -- eqs. (9.2.1)-(9.2.3), pp. 314-316
}
\arguments{
\item{x}{Argument \code{x}; see Usage.}
\item{y}{Argument \code{y}; see Usage.}
}
\value{
A list with \code{statistic}, \code{mean}, \code{var}, \code{var_general}, \code{z}, \code{p_value}, \code{m}, \code{n}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
rmorie:::Moodscale(V, V)
}
