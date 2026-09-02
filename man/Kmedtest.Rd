% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{Kmedtest}
\alias{Kmedtest}
\title{k-sample median test -- Sec. 10.2, pp. 344-346}
\usage{
Kmedtest(samples) 
}
\description{
k-sample median test -- Sec. 10.2, pp. 344-346
}
\arguments{
\item{samples}{Argument \code{samples}; see Usage.}
}
\value{
A list with \code{statistic}, \code{df}, \code{p_value}, \code{u}, \code{t}, \code{median}, \code{prob}, \code{k}, \code{n}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
rmorie:::Kmedtest(V)
}
