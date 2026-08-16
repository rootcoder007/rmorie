% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_burkov_logistic}
\alias{morie_burkov_logistic}
\title{Logistic regression sigma(w.x + b) (Burkov Eq 1.8)}
\usage{
morie_burkov_logistic(w, x, b)
}
\arguments{
\item{w,x}{Vectors.}
\item{b}{Bias.}
}
\description{
Logistic regression sigma(w.x + b) (Burkov Eq 1.8)
}
\value{
A list with \code{estimate}, \code{logit}, \code{predicted_class}, \code{n}, \code{method}.
}
\keyword{internal}
