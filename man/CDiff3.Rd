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
\item{x}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{T}{Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{1}.}
\item{n}{Optional; may be \code{NULL}. Coerced to integer by the body, with \code{as.integer}.}
}
\description{
controls the noise amplification.  The book warns the price is accuracy -- the approximation to d/dt "is poor after about fs/10".
}
\value{
A list with \code{y}, \code{value}, \code{index}, \code{T}, \code{as_averaged_first_differences}, \code{max_difference}, \code{derivation_agrees}, \code{controls_noise_amplification}, \code{poor_above_fs_over_10}, \code{method}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
CDiff3(V)
}
