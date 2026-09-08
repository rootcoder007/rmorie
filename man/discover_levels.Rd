% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{discover_levels}
\alias{discover_levels}
\title{Eq. (9.2)-(9.3): the levels that minimise and maximise the mean}
\usage{
discover_levels(y, A, W, levels, rows = NULL, eval_rows = NULL, ridge = 1e-08) 
}
\arguments{
\item{y}{Outcome vector.}
\item{A}{Exposure vector.}
\item{W}{Covariate matrix.}
\item{levels}{Candidate levels.}
\item{rows}{Rows to fit Q on.}
\item{eval_rows}{Rows the mean is taken over.}
\item{ridge}{Ridge regulariser.}
}
\description{
predicted outcome
}
\value{
\code{aL}, \code{aH}, info with \code{means} and   \code{spread}.
}
\keyword{internal}
\examples{
set.seed(1)
n <- 60
W <- matrix(rnorm(n * 2), n, 2)
A <- sample(0:2, n, replace = TRUE)
y <- plogis(0.5 * A + W[, 1] + rnorm(n, 0, 0.4))
r <- discover_levels(y, A, W, levels = 0:2)
str(r, max.level = 1)
}
