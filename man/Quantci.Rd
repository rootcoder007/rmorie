% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{Quantci}
\alias{Quantci}
\title{Distribution-free quantile confidence interval -- Sec. 5.2, p. 158}
\usage{
Quantci(x, p, r, s)
}
\description{
Distribution-free quantile confidence interval -- Sec. 5.2, p. 158
}
\value{
A list with \code{lower}, \code{upper}, \code{coverage}, \code{alpha}, \code{r}, \code{s}, \code{n}, \code{p}.
}
\keyword{internal}
\examples{
set.seed(1)
x <- rnorm(20)
Quantci(x, p = 0.5, r = 6, s = 15)
}
