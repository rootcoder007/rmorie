% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.tail}
\alias{.tail}
\title{Endpoint weight that absorbs the truncated tail}
\usage{
.tail(B, m)
}
\arguments{
\item{B}{Numeric vector of ideal weights.}
\item{m}{Integer, distance from the centre.}
}
\description{
\code{tilde_B_m = -1/2 B_0 - sum_{j=1}^{m-1} B_j}, footnote 3 of Christiano & Fitzgerald (2003).
}
\value{
A single numeric.
}
\keyword{internal}
\examples{
.tail(B = 5L, m = 5L)
}
