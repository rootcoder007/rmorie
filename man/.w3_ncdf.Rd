% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.w3_ncdf}
\alias{.w3_ncdf}
\title{Standard normal CDF via the incomplete gamma}
\usage{
.w3_ncdf(z)
}
\arguments{
\item{z}{See Usage.}
}
\description{
Phi(z) = 1/2 (1 + sign(z) P(1/2, z^2/2)) is an identity, not an approximation, so this is exactly as accurate as gammp -- and it runs the same series as the Python arm rather than R\\\'s own pnorm.
}
\value{
One of two values, depending on the branch taken.
}
\keyword{internal}
