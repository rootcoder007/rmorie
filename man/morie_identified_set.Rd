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
\item{Y}{Passed to \code{morie_sequence_frequencies}.}
\item{x}{Passed to \code{morie_in_identified_set}.}
\item{beta_grid}{See Usage.}
\item{gamma_grid}{See Usage.}
\item{alpha_grid}{Passed to \code{morie_in_identified_set}.}
\item{beta_fixed}{Optional; may be \code{NULL}. Coerced to numeric by the body, with \code{as.numeric}.}
\item{link}{Passed to \code{morie_in_identified_set}. Defaults to \code{"logit"}.}
\item{tol}{Passed to \code{morie_in_identified_set}. Defaults to \code{0.001}.}
}
\description{
A step of the bnshrt_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A list with \code{estimate}, \code{set}, \code{n_feasible}, \code{beta_bounds}, \code{gamma_bounds}, \code{beta_width}, \code{gamma_width}, \code{point_identified}, \code{discrepancy}, \code{method}, \code{assumes}.
}
\keyword{internal}
\examples{
set.seed(2)
x <- matrix(c(0.5, -0.2, 0.1, 0.4), 2, 2)
Y <- matrix(rbinom(60, 1, 0.5), 30, 2)
r <- morie_identified_set(Y, x, beta_grid = list(c(0.5, -0.3)),
                          gamma_grid = c(0, 0.8),
                          alpha_grid = c(-0.5, 0, 0.5))
str(r, max.level = 1)
}
