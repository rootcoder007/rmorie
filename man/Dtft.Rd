% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{Dtft}
\alias{Dtft}
\title{Eq (3.78): discrete signal, CONTINUOUS frequency -- that is the whole}
\usage{
Dtft(x, omega, n0 = 0) 
}
\arguments{
\item{x}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{omega}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{n0}{Numeric; combined arithmetically in the body. Defaults to \code{0}.}
}
\description{
distinction from the DFT of eq (3.80), which samples this at N points.
}
\value{
A list with \code{X}, \code{omega}, \code{n0}, \code{n}, \code{method}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
Dtft(V, V)
}
