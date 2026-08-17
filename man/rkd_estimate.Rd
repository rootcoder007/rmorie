% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{rkd_estimate}
\alias{rkd_estimate}
\title{rkd_estimate}
\usage{
rkd_estimate(V, Y, kink, bandwidth, order = 2L, kernel = "triangular", policy_slope_change = NULL, B = NULL, fuzzy = FALSE)
}
\arguments{
\item{V}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{Y}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{kink}{Passed to \code{.side_fit}.}
\item{bandwidth}{Passed to \code{.side_fit}.}
\item{order}{Coerced to integer by the body, with \code{as.integer}. Defaults to \code{2L}.}
\item{kernel}{Passed to \code{.side_fit}. Defaults to \code{"triangular"}.}
\item{policy_slope_change}{Optional; may be \code{NULL}. Coerced to numeric by the body, with \code{as.numeric}.}
\item{B}{Optional; may be \code{NULL}. Coerced to numeric by the body, with \code{as.numeric}.}
\item{fuzzy}{A flag; the body branches on it. Defaults to \code{FALSE}.}
}
\description{
A step of the bnskt2_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A list with \code{estimate}, \code{tau}, \code{outcome_kink}, \code{policy_kink}, \code{slope_right}, \code{slope_left}, \code{n_right}, \code{n_left}, \code{bandwidth}, \code{order}, \code{kernel}, \code{fuzzy}, \code{denominator_source}, \code{method}, \code{requires}.
}
\keyword{internal}
\examples{
set.seed(1)
v <- runif(300, -1, 1)
y <- 1 + 0.5 * v + 0.8 * pmax(v, 0) + rnorm(300, 0, 0.1)
r <- rkd_estimate(v, y, kink = 0, bandwidth = 0.5,
                  policy_slope_change = 1)
str(r, max.level = 1)
}
