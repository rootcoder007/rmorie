% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{LsiParH}
\alias{LsiParH}
\title{Eq (3.49): parallel branches add their impulse responses, and the}
\usage{
LsiParH(h_1, h_2, n = NULL)
}
\arguments{
\item{h_1}{See Usage.}
\item{h_2}{See Usage.}
\item{n}{Defaults to \code{NULL}.}
}
\description{
result is as long as the LONGER branch -- not longer, the contrast with the series case where the lengths add.
}
\value{
A list with \code{h}, \code{n_taps}, \code{value}, \code{index}, \code{length_is_the_longer_branch}, \code{method}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
LsiParH(V, V)
}
