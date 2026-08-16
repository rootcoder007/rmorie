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
\item{d}{See Usage.}
\item{r}{See Usage.}
\item{nugget_ratio}{See Usage.}
\item{rng}{See Usage.}
\item{model}{See Usage.}
}
\description{
dR/da is the same expression the Gauss-Newton Jacobian uses, so both fitters share one derivation.
}
\value{
The value of \code{list}.
}
\keyword{internal}
