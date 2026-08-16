% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.s03quantile7}
\alias{.s03quantile7}
\title{Type-7 quantile, the default of R\\'s quantile()}
\usage{
.s03quantile7(v, p)
}
\arguments{
\item{v}{Numeric; passed to \code{sort}.}
\item{p}{Numeric; combined arithmetically in the body.}
}
\description{
A step of the helpers_s03 implementation. Called by \code{.cstat_uno}, \code{.dnnheadweights}, \code{.ot_quantiles} and 38 others in the module. See the file header for the source the module follows.
}
\value{
A numeric value.
}
\keyword{internal}
\examples{
.s03quantile7(v = c(1, 2, 3, 4, 5, 6, 7, 8), p = 0.5)
}
