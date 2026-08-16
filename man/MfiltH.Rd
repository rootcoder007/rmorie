% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{MfiltH}
\alias{MfiltH}
\title{H(n) = g(N-1-n): the template reversed in time, which makes the}
\usage{
MfiltH(g, normalize = FALSE)
}
\arguments{
\item{g}{See Usage.}
\item{normalize}{A flag; the body branches on it. Defaults to \code{FALSE}.}
}
\description{
filter\'s output the cross-correlation with the template.  Reversal is the whole content -- convolving with the unreversed template correlates with a mirrored pattern and peaks in the wrong place.
}
\value{
A list with \code{h}, \code{template}, \code{n}, \code{energy}, \code{normalized}, \code{peak_index}, \code{time_reversed}, \code{output_is_the_cross_correlation}, \code{method}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
MfiltH(V)
}
