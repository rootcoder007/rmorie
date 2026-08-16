% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_cpads_recode_yn}
\alias{.morie_cpads_recode_yn}
\title{Recode a vector using a named map; values absent from the map become `NA`.}
\usage{
.morie_cpads_recode_yn(x)
}
\description{
Codes 98 / 99 (CPADS "don\'t know" / "refused") collapse to `NA`.
}
\value{
The value of \code{out}, as built in the body.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.morie_cpads_recode_yn(V)
}
