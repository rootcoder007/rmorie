% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.lyapun_ols_slope}
\alias{.lyapun_ols_slope}
\title{.lyapun_ols_slope}
\usage{
.lyapun_ols_slope(xs, ys)
}
\arguments{
\item{xs}{A vector; its length is taken and its elements indexed.}
\item{ys}{A vector; indexed elementwise.}
}
\description{
A step of the lyapun_native implementation. Called by \code{lyapunov_exponent}. See the file header for the source the module follows.
}
\value{
A list with \code{slope}, \code{intercept}, \code{se}, \code{r2}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.lyapun_ols_slope(V, V)
}
