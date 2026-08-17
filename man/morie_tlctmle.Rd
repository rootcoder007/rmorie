% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_tlctmle}
\alias{morie_tlctmle}
\title{morie_tlctmle}
\usage{
morie_tlctmle(A, Y, Q1, Q0, W, g_models, V = 5L, seed = 0L, penalty = TRUE)
}
\arguments{
\item{A}{Passed to \code{ctmle}.}
\item{Y}{Passed to \code{ctmle}.}
\item{Q1}{Passed to \code{ctmle}.}
\item{Q0}{Passed to \code{ctmle}.}
\item{W}{Passed to \code{ctmle}.}
\item{g_models}{Passed to \code{ctmle}.}
\item{V}{Passed to \code{ctmle}. Defaults to \code{5L}.}
\item{seed}{Passed to \code{ctmle}. Defaults to \code{0L}.}
\item{penalty}{Passed to \code{ctmle}. Defaults to \code{TRUE}.}
}
\description{
A step of the tlctmle_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
The value of \code{ctmle}.
}
\keyword{internal}
\examples{
set.seed(1)
n <- 100
W <- matrix(rnorm(n * 3), n, 3)
A <- rbinom(n, 1, plogis(W[, 1]))
Y <- rbinom(n, 1, plogis(0.5 * A + W[, 2]))
Q1 <- plogis(0.5 + W[, 2])
Q0 <- plogis(W[, 2])
g_models <- list(1, c(1, 2), c(1, 2, 3))
morie_tlctmle(A, Y, Q1, Q0, W, g_models, V = 3L)
}
