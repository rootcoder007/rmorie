% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{Whopf}
\alias{Whopf}
\title{Eqs. (3.168), (3.171): build the Toeplitz correlation matrix and the}
\usage{
Whopf(x, d, order)
}
\arguments{
\item{x}{See Usage.}
\item{d}{See Usage.}
\item{order}{See Usage.}
}
\description{
cross-correlation vector from data and solve for the tap weights. The biased (1/N) ACF estimate is used, which is what makes Phi nonnegative-definite and so keeps the system solvable.
}
\value{
A list with \code{w}, \code{phi}, \code{theta}, \code{Phi}, \code{order}, \code{j_min}, \code{var_d}, \code{toeplitz}, \code{acf_biased}, \code{condition}, \code{method}.
}
\keyword{internal}
