% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.drift_adjust}
\alias{.drift_adjust}
\title{Drift adjustment for the asymmetric filter}
\usage{
.drift_adjust(x)
}
\arguments{
\item{x}{Numeric vector.}
}
\description{
Removes the random walk\'s drift \code{mu_hat = (x_T - x_1) / (T - 1)} from the series. The asymmetric filter has one unit root and is not drift-invariant; the symmetric one has two and does not need it.
}
\value{
A list with \code{adjusted} (the detrended series) and   \code{drift} (the removed drift).
}
\keyword{internal}
