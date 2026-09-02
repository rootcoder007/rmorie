% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{AncInput}
\alias{AncInput}
\title{Section 3.10.1: the primary input is x = v + m.  The method needs the}
\usage{
AncInput(v, m) 
}
\arguments{
\item{v}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{m}{Coerced to numeric by the body, with \code{as.numeric}.}
}
\description{
signal and the interference statistically independent; the sample correlation between them is returned so that premise is testable instead of assumed.
}
\value{
A list with \code{x}, \code{v}, \code{m}, \code{n}, \code{correlation}, \code{independent}, \code{assumption}, \code{method}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
AncInput(V, V)
}
