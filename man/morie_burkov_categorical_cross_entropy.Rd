% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_burkov_categorical_cross_entropy}
\alias{morie_burkov_categorical_cross_entropy}
\title{Categorical cross-entropy with a one-hot target (Burkov Eq 2.1)}
\usage{
morie_burkov_categorical_cross_entropy(y_hat, c) 
}
\arguments{
\item{y_hat}{Probability distribution.}
\item{c}{Correct class,   0-based to match the Python mirror.}
}
\description{
Categorical cross-entropy with a one-hot target (Burkov Eq 2.1)
}
\value{
A list with \code{estimate}, \code{p_correct}, \code{n_classes}, \code{n}, \code{method}.
}
\keyword{internal}
\examples{
morie_burkov_categorical_cross_entropy(c(0.7, 0.2, 0.1), 0)
}
