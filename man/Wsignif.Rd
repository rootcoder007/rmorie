% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{Wsignif}
\alias{Wsignif}
\title{Concordance W significance test -- Sec. 12.4.2, p. 455}
\usage{
Wsignif(w, k, n)
}
\description{
Concordance W significance test -- Sec. 12.4.2, p. 455
}
\value{
A list with \code{statistic}, \code{w}, \code{df}, \code{p_value}, \code{s}, \code{table_n}, \code{k}, \code{n}.
}
\keyword{internal}
\examples{
Wsignif(w = 0.6, k = 3, n = 10)
}
