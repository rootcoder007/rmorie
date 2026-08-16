% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.schab_aic_from_parts}
\alias{.schab_aic_from_parts}
\title{Fotheringham et al. (2002) p. 96 eq (4.22)}
\usage{
.schab_aic_from_parts(n, sigma2, tr_S)
}
\arguments{
\item{n}{Numeric; combined arithmetically in the body.}
\item{sigma2}{Numeric; passed to \code{sqrt}.}
\item{tr_S}{Coerced to numeric by the body, with \code{as.numeric}.}
}
\description{
A step of the schab_gwr_shared implementation. Called by \code{.schab_gwr_criterion}, \code{spgwrb}. See the file header for the source the module follows.
}
\value{
A numeric value.
}
\keyword{internal}
