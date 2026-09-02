% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{Ks2}
\alias{Ks2}
\title{Two-sample KS test -- Sec. 6.3, p. 239}
\usage{
Ks2(x, y) 
}
\description{
Two-sample KS test -- Sec. 6.3, p. 239
}
\arguments{
\item{x}{Argument \code{x}; see Usage.}
\item{y}{Argument \code{y}; see Usage.}
}
\value{
A list with \code{statistic}, \code{p_value}, \code{dplus}, \code{dminus}, \code{m}, \code{n}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
Ks2(V, V)
}
