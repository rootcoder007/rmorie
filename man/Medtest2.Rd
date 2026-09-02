% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{Medtest2}
\alias{Medtest2}
\title{Two-sided median test with exact region -- Sec. 6.4, p. 247}
\usage{
Medtest2(x, y, alpha = 0.05) 
}
\description{
Two-sided median test with exact region -- Sec. 6.4, p. 247
}
\arguments{
\item{x}{Argument \code{x}; see Usage.}
\item{y}{Argument \code{y}; see Usage.}
\item{alpha}{Argument \code{alpha}; see Usage.}
}
\value{
A list with \code{statistic}, \code{p_value}, \code{lower}, \code{upper}, \code{alpha_exact}, \code{t}, \code{m}, \code{n}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
rmorie:::Medtest2(V, V)
}
