% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_cpads_to_numeric}
\alias{.morie_cpads_to_numeric}
\title{Coerce numeric, mapping non-numeric strings to `NA_real_` (silent).}
\usage{
.morie_cpads_to_numeric(x)
}
\description{
Coerce numeric, mapping non-numeric strings to `NA_real_` (silent).
}
\value{
The value of \code{suppressWarnings}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.morie_cpads_to_numeric(V)
}
