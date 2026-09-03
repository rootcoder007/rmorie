% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{Taupartial}
\alias{Taupartial}
\title{Kendall partial tau -- Sec. 12.6, eq. (12.6.1), p. 467}
\usage{
Taupartial(x, y, z) 
}
\description{
Kendall partial tau -- Sec. 12.6, eq. (12.6.1), p. 467
}
\arguments{
\item{x}{Argument \code{x}; see Usage.}
\item{y}{Argument \code{y}; see Usage.}
\item{z}{Argument \code{z}; see Usage.}
}
\value{
A list with \code{statistic}, \code{x11}, \code{x12}, \code{x21}, \code{x22}, \code{dropped}, \code{npairs}, \code{n}.
}
\keyword{internal}
\examples{
rmorie:::Taupartial(x = c(1, 2, 3, 4, 5, 6, 7, 8), y = c(1, 2, 3, 4, 5, 6, 7, 8),
  z = c(1, 2, 3, 4, 5, 6, 7, 8))
}
