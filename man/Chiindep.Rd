% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{Chiindep}
\alias{Chiindep}
\title{Chi-square test of independence -- Sec. 14.2, p. 505}
\usage{
Chiindep(table, correct = FALSE)
}
\description{
Chi-square test of independence -- Sec. 14.2, p. 505
}
\value{
A list with \code{statistic}, \code{df}, \code{p_value}, \code{expected}, \code{n}, \code{r}, \code{c}.
}
\keyword{internal}
\examples{
M <- matrix(c(1, 2, 3, 4, 5, 6), nrow = 2)
Chiindep(M)
}
