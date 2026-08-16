% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.gpzapcpredict}
\alias{.gpzapcpredict}
\title{.gpzapcpredict}
\usage{
.gpzapcpredict(theta_hat, mu_hat, threshold = 0.5)
}
\arguments{
\item{theta_hat}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{mu_hat}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{threshold}{Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{0.5}.}
}
\description{
A step of the helpers_gp_core implementation. Called by \code{Msm329}. See the file header for the source the module follows.
}
\value{
One of two values, depending on the branch taken.
}
\keyword{internal}
