% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{Fisherex}
\alias{Fisherex}
\title{Fisher exact test -- Sec. 14.4, p. 517}
\usage{
Fisherex(table, alternative = "two-sided")
}
\description{
Fisher exact test -- Sec. 14.4, p. 517
}
\value{
A list with \code{p_value}, \code{p_greater}, \code{p_less}, \code{prob}, \code{statistic}, \code{support}.
}
\keyword{internal}
\examples{
Fisherex(rbind(c(8, 2), c(1, 5)))
}
