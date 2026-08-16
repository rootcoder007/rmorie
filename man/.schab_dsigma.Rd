% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.schab_dsigma}
\alias{.schab_dsigma}
\title{DSigma(theta*)/d(xi, a): d/dxi is I - R, d/da is (1 - xi) dR/da.}
\usage{
.schab_dsigma(d, r, nugget_ratio, rng, model)
}
\arguments{
\item{d}{A matrix; passed to \code{nrow}.}
\item{r}{Numeric; combined arithmetically in the body.}
\item{nugget_ratio}{Numeric; combined arithmetically in the body.}
\item{rng}{Passed to \code{.schab_semivariogram_jacobian}.}
\item{model}{Passed to \code{.schab_semivariogram_jacobian}.}
}
\description{
dR/da is the same expression the Gauss-Newton Jacobian uses, so both fitters share one derivation.
}
\value{
The value of \code{list}.
}
\keyword{internal}
