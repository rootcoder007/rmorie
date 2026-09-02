% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{Lillieexp}
\alias{Lillieexp}
\title{Lilliefors test for exponentiality -- Sec. 4.6, p. 133; Table T}
\usage{
Lillieexp(x, alpha = 0.05) 
}
\description{
Lilliefors test for exponentiality -- Sec. 4.6, p. 133; Table T
}
\arguments{
\item{x}{Argument \code{x}; see Usage.}
\item{alpha}{Argument \code{alpha}; see Usage.}
}
\value{
A list with \code{statistic}, \code{dcrit}, \code{reject}, \code{mean}, \code{n}, \code{n_table}, \code{alpha}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
rmorie:::Lillieexp(V)
}
