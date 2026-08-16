% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.sp_k}
\alias{.sp_k}
\title{Internal: Ripley\'s K estimated as in Sec 3.4.2.}
\usage{
.sp_k(points, region, r, correction = "border")
}
\arguments{
\item{points}{Event coordinates (n by 2).}
\item{region}{Bounding region.}
\item{r}{Numeric vector of distances.}
\item{correction}{"border" or "none".}
}
\description{
E_tilde(h) = (1/n) sum_i sum_{j != i} I(h_ij <= h); K = E / lambda_hat. The naive form is negatively biased because events outside the window are unobserved, so the border correction keeps only events further than h from the boundary.
}
\value{
Numeric vector K(r).
}
\keyword{internal}
