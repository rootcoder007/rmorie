% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_burkov_layer1_output}
\alias{morie_burkov_layer1_output}
\title{First hidden layer phi(W1 x + b1) (Burkov Eq 1.6)}
\usage{
morie_burkov_layer1_output(W_1, x, b_1, phi = "relu") 
}
\arguments{
\item{W_1}{Weight matrix.}
\item{x}{Input.}
\item{b_1}{Bias.}
\item{phi}{Activation name or function.}
}
\description{
First hidden layer phi(W1 x + b1) (Burkov Eq 1.6)
}
\value{
A list with \code{output}, \code{preactivation}, \code{estimate}, \code{n}, \code{method}.
}
\keyword{internal}
\examples{
morie_burkov_layer1_output(W_1 = c(1, 2, 3, 4, 5, 6, 7, 8), x = 5L, b_1 = c(1, 2, 3, 4, 5, 6, 7, 8))
}
