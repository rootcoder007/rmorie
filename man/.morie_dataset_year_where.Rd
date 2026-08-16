% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_dataset_year_where}
\alias{.morie_dataset_year_where}
\title{Build a SQL-ish `WHERE` clause for an "OCC_YEAR = ?" or "1=1" filter.}
\usage{
.morie_dataset_year_where(year)
}
\description{
Build a SQL-ish `WHERE` clause for an "OCC_YEAR = ?" or "1=1" filter.
}
\value{
A character value.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.morie_dataset_year_where(V)
}
