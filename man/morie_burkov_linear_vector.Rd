% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_burkov_linear_vector}
\alias{morie_burkov_linear_vector}
\title{Vector-form linear model w.x + b (Burkov Eq 1.4)}
\usage{
morie_burkov_linear_vector(w, x, b) 
}
\arguments{
\item{w,x}{Vectors.}
\item{b}{Bias.}
}
\description{
Vector-form linear model w.x + b (Burkov Eq 1.4)
}
\value{
A list with \code{estimate}, \code{dot}, \code{b}, \code{n}, \code{method}.
}
\keyword{internal}
\examples{
morie_burkov_linear_vector(w = c(1, 2, 3, 4, 5, 6, 7, 8), x = c(1, 2, 3, 4, 5, 6, 7, 8), b = 5L)
}
