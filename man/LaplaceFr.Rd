% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{LaplaceFr}
\alias{LaplaceFr}
\title{Eq (3.52): H(omega) = H(s) at s = j omega.  Legitimate only when the}
\usage{
LaplaceFr(h, omega, t = NULL, T = NULL)
}
\arguments{
\item{h}{See Usage.}
\item{omega}{See Usage.}
\item{t}{Defaults to \code{NULL}.}
\item{T}{Defaults to \code{NULL}.}
}
\description{
imaginary axis lies inside the region of convergence, which holds for a causal stable system and fails for an unstable one.
}
\value{
A list with \code{H}, \code{omega}, \code{magnitude}, \code{phase}, \code{t_min}, \code{t_max}, \code{valid_only_inside_the_roc}, \code{method}.
}
\keyword{internal}
