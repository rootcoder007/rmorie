% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_km_estimate}
\alias{.morie_km_estimate}
\title{.morie_km_estimate}
\usage{
.morie_km_estimate(t, e)
}
\arguments{
\item{t}{A vector; indexed elementwise.}
\item{e}{See Usage.}
}
\description{
A step of the cox_native implementation. Called by \code{morie_censoring_at_risk_weight}, \code{morie_competing_risks_fg}, \code{morie_cox_schoenfeld_residuals}. See the file header for the source the module follows.
}
\value{
A list with \code{times}, \code{survival}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.morie_km_estimate(V, V)
}
