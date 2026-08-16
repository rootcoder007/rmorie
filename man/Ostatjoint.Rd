% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{Ostatjoint}
\alias{Ostatjoint}
\title{Joint density of X_(r), X_(s) -- Sec. 2.5, p. 39}
\usage{
Ostatjoint(x, y, r, s, n, cdf, pdf)
}
\description{
Joint density of X_(r), X_(s) -- Sec. 2.5, p. 39
}
\value{
A list with \code{pdf}, \code{coef}, \code{fx}, \code{fy}, \code{r}, \code{s}, \code{n}.
}
\keyword{internal}
\examples{
Ostatjoint(x = -0.5, y = 0.5, r = 1, s = 3, n = 5,
           cdf = pnorm, pdf = dnorm)
}
