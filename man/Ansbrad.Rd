% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{Ansbrad}
\alias{Ansbrad}
\title{Freund-Ansari-Bradley-David-Barton test -- eq. (9.3.1), p. 316}
\usage{
Ansbrad(x, y) 
}
\description{
Freund-Ansari-Bradley-David-Barton test -- eq. (9.3.1), p. 316
}
\arguments{
\item{x}{Argument \code{x}; see Usage.}
\item{y}{Argument \code{y}; see Usage.}
}
\value{
A list with \code{statistic}, \code{mean}, \code{var}, \code{z}, \code{p_value}, \code{scores}, \code{m}, \code{n}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
rmorie:::Ansbrad(V, V)
}
