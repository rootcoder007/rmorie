% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{Absrank}
\alias{Absrank}
\title{Midranks of |d| with signs -- Sec. 5.5, p. 189}
\usage{
Absrank(d) 
}
\description{
Midranks of |d| with signs -- Sec. 5.5, p. 189
}
\arguments{
\item{d}{Argument \code{d}; see Usage.}
}
\value{
A list with \code{ranks}, \code{signs}, \code{signed}, \code{ties}, \code{n}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
rmorie:::Absrank(V)
}
