% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{HannMag}
\alias{HannMag}
\title{Eq (3.106): unity at DC, exactly nought at omega = pi, monotone}
\usage{
HannMag(omega)
}
\arguments{
\item{omega}{See Usage.}
}
\description{
between.  The book\'s absolute value is redundant -- 1 + cos is never negative -- and is kept only because the book writes it.
}
\value{
A list with \code{magnitude}, \code{omega}, \code{dc_gain}, \code{nyquist_gain}, \code{lowpass}, \code{absolute_value_is_redundant}, \code{method}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
HannMag(V)
}
