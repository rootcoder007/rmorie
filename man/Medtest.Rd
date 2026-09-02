% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{Medtest}
\alias{Medtest}
\title{Two-sample median test -- Sec. 6.4, p. 247}
\usage{
Medtest(x, y) 
}
\description{
Two-sample median test -- Sec. 6.4, p. 247
}
\arguments{
\item{x}{Argument \code{x}; see Usage.}
\item{y}{Argument \code{y}; see Usage.}
}
\value{
A list with \code{statistic}, \code{p_value}, \code{p_lower}, \code{p_upper}, \code{median}, \code{t}, \code{mean}, \code{var}, \code{m}, \code{n}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
Medtest(V, V)
}
