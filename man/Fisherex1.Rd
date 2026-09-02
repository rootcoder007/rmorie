% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{Fisherex1}
\alias{Fisherex1}
\title{One-sided Fisher exact test -- Sec. 14.4, p. 517}
\usage{
Fisherex1(table, alternative = "greater") 
}
\description{
One-sided Fisher exact test -- Sec. 14.4, p. 517
}
\arguments{
\item{table}{Argument \code{table}; see Usage.}
\item{alternative}{Argument \code{alternative}; see Usage.}
}
\value{
A list with \code{p_value}, \code{p_greater}, \code{p_less}, \code{prob}, \code{statistic}, \code{mean}.
}
\keyword{internal}
\examples{
Fisherex1(rbind(c(8, 2), c(1, 5)))
}
