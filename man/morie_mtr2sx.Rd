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
\item{beta_x}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{se_x}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{beta_y}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{se_y}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{model}{One of \code{"fixed"}, \code{"multiplicative"}. Defaults to \code{"multiplicative"}.}
\item{weights}{Compared against \code{"second_order"}. Defaults to \code{"first_order"}.}
\item{theta}{Numeric; combined arithmetically in the body. Defaults to \code{0}.}
}
\description{
A step of the mtr2sx_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A list with \code{estimate}, \code{se}, \code{z}, \code{p_value}, \code{ci}, \code{ratio_estimates}, \code{variances}, \code{weights_used}, \code{model}, \code{phi_multiplicative}, \code{tau2}, \code{Q}, \code{df}, \code{I2}, \code{se_fixed}, \code{regression_estimate}, \code{regression_se_fixed}, \code{n_variants}, \code{method}.
}
\keyword{internal}
\examples{
morie_mtr2sx(beta_x = c(1, 2, 3, 4, 5, 6, 7, 8), se_x = c(1, 2, 3, 4, 5, 6, 7, 8), beta_y = c(1, 2, 3, 4, 5, 6, 7, 8), se_y = c(1, 2, 3, 4, 5, 6, 7, 8))
}
