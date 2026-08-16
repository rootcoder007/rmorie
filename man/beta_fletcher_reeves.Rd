% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{beta_fletcher_reeves}
\alias{beta_fletcher_reeves}
\title{beta_fletcher_reeves}
\usage{
beta_fletcher_reeves(g_new, g_old)
}
\arguments{
\item{g_new}{Passed to \code{.cgnonl_dot}.}
\item{g_old}{Passed to \code{.cgnonl_dot}.}
}
\description{
A step of the cgnonl_native implementation. Called by \code{.cgnonl_beta}. See the file header for the source the module follows.
}
\value{
A numeric value.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
beta_fletcher_reeves(V, V)
}
