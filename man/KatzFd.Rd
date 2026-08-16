% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{KatzFd}
\alias{KatzFd}
\title{Katz (1988): FD = log10(n) / (log10(n) + log10(d/L)), with L the path}
\usage{
KatzFd(x, dt = 1)
}
\arguments{
\item{x}{See Usage.}
\item{dt}{Defaults to \code{1}.}
}
\description{
length, d the greatest distance from the first point, n = L/a. Rangayyan (2024) Section 5.13.2 covers the ruler, box-counting and Higuchi methods instead, so this is cited to Katz.  It mixes the amplitude and time axes, so rescaling the signal changes the answer.
}
\value{
A list with \code{fd}, \code{total_length}, \code{max_distance}, \code{mean_step}, \code{n_steps}, \code{n}, \code{scale_sensitive}, \code{method}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
KatzFd(V)
}
