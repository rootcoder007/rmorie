% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{MantelM2}
\alias{MantelM2}
\title{Eqs (1.4) and (1.5) with the book\'s own default choices,}
\usage{
MantelM2(coords, x, w = NULL, u = NULL) 
}
\arguments{
\item{coords}{A matrix; passed to \code{as.matrix}.}
\item{x}{Passed to \code{.morie_spx_chkv}.}
\item{w}{Optional; may be \code{NULL}. Passed to \code{.morie_spx_chkw}.}
\item{u}{Optional; may be \code{NULL}. Passed to \code{.morie_spx_chkw}.}
}
\description{
W_ij = ||s_i - s_j|| and U_ij = |Z_i - Z_j|, plus the regression slope beta = M2 / sum sum W_ij^2 displayed in Sec. 1.3.1.
}
\value{
A list with \code{m1}, \code{m2}, \code{beta}, \code{sw2}, \code{s0}, \code{mean_attribute}, \code{n}, \code{method}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
MantelM2(V, V)
}
