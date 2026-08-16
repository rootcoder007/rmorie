% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_multinomial_block}
\alias{morie_multinomial_block}
\title{morie_multinomial_block}
\usage{
morie_multinomial_block(X, y, beta0, beta, lambda, cls, baseline_last = TRUE)
}
\description{
morie_multinomial_block
}
\value{
A list with \code{beta0}, \code{beta}, \code{weights}, \code{working_response}.
}
\keyword{internal}
\examples{
set.seed(7)
X <- matrix(rnorm(60), 30, 2)
y <- sample(0:2, 30, replace = TRUE)
beta0 <- rep(0, 3)
beta <- matrix(0, 3, 2)
morie_multinomial_block(X, y, beta0, beta, lambda = 0.1, cls = 0L)
}
