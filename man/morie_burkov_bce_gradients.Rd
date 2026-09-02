% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_burkov_bce_gradients}
\alias{morie_burkov_bce_gradients}
\title{Closed-form BCE gradients for logistic regression (Burkov Eq 1.11)}
\usage{
morie_burkov_bce_gradients(y_hat, y, x, N = NULL, j = NULL) 
}
\arguments{
\item{y_hat,y}{Predictions and targets.}
\item{x}{Design matrix, one   row per example.}
\item{N}{Optional size check.}
\item{j}{Optional   0-based coordinate for `estimate` (matching the Python mirror).}
}
\description{
Closed-form BCE gradients for logistic regression (Burkov Eq 1.11)
}
\value{
A list with \code{grad_w}, \code{grad_b}, \code{estimate}, \code{n}, \code{method}.
}
\keyword{internal}
\examples{
morie_burkov_bce_gradients(y_hat = c(1, 2, 3, 4, 5, 6, 7, 8), y = c(1, 2, 3, 4, 5, 6, 7, 8), x = c(1, 2, 3, 4, 5, 6, 7, 8))
}
