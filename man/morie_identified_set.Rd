% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_identified_set}
\alias{morie_identified_set}
\title{morie_identified_set}
\usage{
morie_identified_set(Y, x, beta_grid, gamma_grid, alpha_grid, beta_fixed = NULL, link = "logit", tol = 0.001)
}
\arguments{
\item{Y}{See Usage.}
\item{x}{See Usage.}
\item{beta_grid}{See Usage.}
\item{gamma_grid}{See Usage.}
\item{alpha_grid}{See Usage.}
\item{beta_fixed}{Defaults to \code{NULL}.}
\item{link}{Defaults to \code{"logit"}.}
\item{tol}{Defaults to \code{0.001}.}
}
\description{
Part of the bnshrt_native implementation; see the file header for the source it follows.
}
\value{
A list with \code{estimate}, \code{set}, \code{n_feasible}, \code{beta_bounds}, \code{gamma_bounds}, \code{beta_width}, \code{gamma_width}, \code{point_identified}, \code{discrepancy}, \code{method}, \code{assumes}.
}
\keyword{internal}
