% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.sp_cov_from_model}
\alias{.sp_cov_from_model}
\title{Internal: covariance C(h) from a nugget / sill / range / model spec.}
\usage{
.sp_cov_from_model(h, cov_model = NULL)
}
\arguments{
\item{h}{Numeric matrix or vector of distances.}
\item{cov_model}{List with `model`, `nugget`, `sill`, `range`.}
}
\description{
C(h) = sill * R(h) for h > 0 and C(0) = nugget + sill: the nugget is a variance component present only at zero lag (Sec 4.3.6).
}
\value{
Numeric object shaped like `h`.
}
\keyword{internal}
