% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.schab_aicc_from_parts}
\alias{.schab_aicc_from_parts}
\title{Charlton white paper p. 8; spgwr::gwr.aic.f;}
\usage{
.schab_aicc_from_parts(n, sigma2, tr_S)
}
\arguments{
\item{n}{Numeric; combined arithmetically in the body.}
\item{sigma2}{Numeric; passed to \code{sqrt}.}
\item{tr_S}{Numeric; combined arithmetically in the body.}
}
\description{
mgwr.diagnostics.get_AICc Fotheringham et al. (2002) p. 61 eq (2.33) / p. 96 eq (4.21).
}
\value{
A numeric value.
}
\keyword{internal}
\examples{
.schab_aicc_from_parts(n = 5L, sigma2 = c(1, 2, 3, 4, 5, 6, 7, 8), tr_S = 5L)
}
