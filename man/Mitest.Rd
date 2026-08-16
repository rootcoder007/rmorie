% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{Mitest}
\alias{Mitest}
\title{Multiply-imputed Wald test (Li et al. 1991)}
\usage{
Mitest(theta, U)
}
\description{
Multiply-imputed Wald test (Li et al. 1991)
}
\value{
A list with \code{statistic}, \code{df1}, \code{df2}, \code{r}, \code{estimate}, \code{m}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
Mitest(V, V)
}
