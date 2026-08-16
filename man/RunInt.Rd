% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{RunInt}
\alias{RunInt}
\title{Eq (3.112): the continuous counterpart of the moving-average sum.}
\usage{
RunInt(x, t, tau)
}
\arguments{
\item{x}{See Usage.}
\item{t}{See Usage.}
\item{tau}{See Usage.}
}
\description{
The window is clipped at the start of the record, and how many windows that affects is reported rather than left to be discovered.
}
\value{
A list with \code{y}, \code{n}, \code{tau}, \code{clipped_windows}, \code{trapezoidal}, \code{continuous_counterpart_of_the_ma_filter}, \code{method}.
}
\keyword{internal}
