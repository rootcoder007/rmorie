% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{density_kink_test}
\alias{density_kink_test}
\title{density_kink_test}
\usage{
density_kink_test(V, kink, bandwidth, n_bins = 20L, order = 1L)
}
\arguments{
\item{V}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{kink}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{bandwidth}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{n_bins}{Coerced to integer by the body, with \code{as.integer}. Defaults to \code{20L}.}
\item{order}{Coerced to integer by the body, with \code{as.integer}. Defaults to \code{1L}.}
}
\description{
A step of the bnskt2_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A list with \code{slope_change}, \code{relative}, \code{slope_right}, \code{slope_left}, \code{n_inside}, \code{n_bins}, \code{smooth}, \code{interpretation}.
}
\keyword{internal}
