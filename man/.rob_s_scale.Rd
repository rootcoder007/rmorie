% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.rob_s_scale}
\alias{.rob_s_scale}
\title{The M-scale: s solving mean(rho(r/s)) = b, biweight at c = 1.5476,}
\usage{
.rob_s_scale(r, cc = .rob_tukey_c_bdp, b = 0.5)
}
\arguments{
\item{r}{Numeric; passed to \code{abs}.}
\item{cc}{Passed to \code{.rob_tukey_rho}. Defaults to \code{.rob_tukey_c_bdp}.}
\item{b}{Numeric; combined arithmetically in the body. Defaults to \code{0.5}.}
}
\description{
b = 1/2 -- the 50\%-breakdown calibration. The fixed-point iteration s^2 <- s^2 * mean(rho(r/s))/b is monotone for the biweight.
}
\value{
The value of \code{s}, as built in the body.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.rob_s_scale(V)
}
