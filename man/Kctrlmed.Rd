% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{Kctrlmed}
\alias{Kctrlmed}
\title{k-sample control median test -- Sec. 10.3, eq. (10.3.1), pp. 350-351}
\usage{
Kctrlmed(samples, p = c(0.5))
}
\description{
k-sample control median test -- Sec. 10.3, eq. (10.3.1), pp. 350-351
}
\value{
A list with \code{counts}, \code{cuts}, \code{r}, \code{pcell}, \code{statistic}, \code{df}, \code{p_value}, \code{k}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
Kctrlmed(V)
}
