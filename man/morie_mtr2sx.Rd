% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_mtr2sx}
\alias{morie_mtr2sx}
\title{morie_mtr2sx}
\usage{
morie_mtr2sx(beta_x, se_x, beta_y, se_y, model = "multiplicative", weights = "first_order", theta = 0)
}
\arguments{
\item{beta_x}{See Usage.}
\item{se_x}{See Usage.}
\item{beta_y}{See Usage.}
\item{se_y}{See Usage.}
\item{model}{Defaults to \code{"multiplicative"}.}
\item{weights}{Defaults to \code{"first_order"}.}
\item{theta}{Defaults to \code{0}.}
}
\description{
Part of the mtr2sx_native implementation; see the file header for the source it follows.
}
\value{
A list with \code{estimate}, \code{se}, \code{z}, \code{p_value}, \code{ci}, \code{ratio_estimates}, \code{variances}, \code{weights_used}, \code{model}, \code{phi_multiplicative}, \code{tau2}, \code{Q}, \code{df}, \code{I2}, \code{se_fixed}, \code{regression_estimate}, \code{regression_se_fixed}, \code{n_variants}, \code{method}.
}
\keyword{internal}
