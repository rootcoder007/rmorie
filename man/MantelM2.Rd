% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{MantelM2}
\alias{MantelM2}
\title{Eqs (1.4) and (1.5) with the book\\'s own default choices,}
\usage{
MantelM2(coords, x, w = NULL, u = NULL)
}
\arguments{
\item{coords}{See Usage.}
\item{x}{See Usage.}
\item{w}{Defaults to \code{NULL}.}
\item{u}{Defaults to \code{NULL}.}
}
\description{
W_ij = ||s_i - s_j|| and U_ij = |Z_i - Z_j|, plus the regression slope beta = M2 / sum sum W_ij^2 displayed in Sec. 1.3.1.
}
\value{
A list with \code{m1}, \code{m2}, \code{beta}, \code{sw2}, \code{s0}, \code{mean_attribute}, \code{n}, \code{method}.
}
\keyword{internal}
