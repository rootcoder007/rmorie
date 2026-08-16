% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{MfOutput}
\alias{MfOutput}
\title{Eq (4.34), computed as y = x * h: exact for a finite record, where}
\usage{
MfOutput(x, h, dt = 1)
}
\arguments{
\item{x}{See Usage.}
\item{h}{See Usage.}
\item{dt}{Defaults to \code{1}.}
}
\description{
the frequency-domain route would need fine enough sampling to avoid wrap-around.  eq (4.38) reads M_y off the peak returned here.
}
\value{
A list with \code{y}, \code{t}, \code{peak_index}, \code{peak_time}, \code{peak_magnitude}, \code{dt}, \code{method}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
MfOutput(V, V)
}
