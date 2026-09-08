% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{SpGam}
\alias{SpGam}
\title{Thin-plate spline eta(r) = r^2 log r plus linear covariates, solved}
\usage{
SpGam(y, x, coords, lam = 0) 
}
\arguments{
\item{y}{Passed to \code{.morie_spx_chkv}.}
\item{x}{Optional; may be \code{NULL}. A matrix; passed to \code{as.matrix}.}
\item{coords}{A matrix; passed to \code{as.matrix}.}
\item{lam}{Numeric; combined arithmetically in the body. Defaults to \code{0}.}
}
\description{
as the saddle-point system [K + n lam I, T; T\', 0].  T = [1, s1, s2, X] spans the null space of the penalty and must NOT be shrunk; dropping the T\'c = 0 block leaves the system singular.  Duchon (1977); Wood (2006) Ch. 4.  NOT in Schabenberger & Gotway, whose parametric analogue is Sec. 5.3.1.
}
\value{
A list with \code{fitted}, \code{residuals}, \code{coef}, \code{spline_weights}, \code{rss}, \code{penalty}, \code{lam}, \code{null_space_is_unpenalised}, \code{n}, \code{method}.
}
\keyword{internal}
\examples{
set.seed(6)
coords <- matrix(runif(24), 12, 2)
x <- rnorm(12)
y <- 1 + sin(x) + rnorm(12, 0, 0.2)
str(SpGam(y, x, coords, lam = 0.1), max.level = 1)
}
