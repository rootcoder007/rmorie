% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{CDiff3}
\alias{CDiff3}
\title{Eq (3.128): the mean of two successive first differences, which}
\usage{
CDiff3(x, T = 1, n = NULL)
}
\arguments{
\item{x}{See Usage.}
\item{T}{Defaults to \code{1}.}
\item{n}{Defaults to \code{NULL}.}
}
\description{
controls the noise amplification.  The book warns the price is accuracy -- the approximation to d/dt "is poor after about fs/10".
}
\value{
A list with \code{y}, \code{value}, \code{index}, \code{T}, \code{as_averaged_first_differences}, \code{max_difference}, \code{derivation_agrees}, \code{controls_noise_amplification}, \code{poor_above_fs_over_10}, \code{method}.
}
\keyword{internal}
