% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.crsfmr_softmax}
\alias{.crsfmr_softmax}
\title{Softmax}
\usage{
.crsfmr_softmax(x)
}
\arguments{
\item{x}{Numeric vector.}
}
\description{
Numerically stable softmax over a numeric vector.
}
\value{
Numeric vector, same length as \code{x}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.crsfmr_softmax(V)
}
