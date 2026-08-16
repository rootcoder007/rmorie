% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.otis_crosstab}
\alias{.otis_crosstab}
\title{Internal helper: Otis Crosstab}
\usage{
.otis_crosstab(df, row, col, value, aggfunc = c("sum", "max"), top_rows = 20L)
}
\description{
Internal helper: Otis Crosstab
}
\value{
A list with \code{title}, \code{headers}, \code{rows}.
}
\keyword{internal}
\examples{
.otis_crosstab(df = data.frame(x = c(1, 2, 3, 4), y = c(2, 4, 5, 9)), row = c(1, 2, 3, 4, 5, 6, 7, 8), col = c("a", "b", "c"), value = c(1, 2, 3, 4, 5, 6, 7, 8))
}
