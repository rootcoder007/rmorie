% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{LinConv}
\alias{LinConv}
\title{Eqs (3.36)-(3.37): y(n) = sum_k x(k) h(n-k) = sum_k h(k) x(n-k),}
\usage{
LinConv(x, h, causal = TRUE)
}
\arguments{
\item{x}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{h}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{causal}{A flag; the body branches on it. Defaults to \code{TRUE}.}
}
\description{
causality assumed as the book states under eq (3.37).  eq (3.39) reads the same sum as delayed, weighted copies of h; those copies are returned so the overlap of Figure 3.19 is visible.
}
\value{
A list with \code{y}, \code{n}, \code{n_x}, \code{n_h}, \code{contributions}, \code{commutes}, \code{causal}, \code{method}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
LinConv(V, V)
}
