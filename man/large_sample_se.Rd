% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{large_sample_se}
\alias{large_sample_se}
\title{large_sample_se}
\usage{
large_sample_se(theta, Theta, n)
}
\arguments{
\item{theta}{See Usage.}
\item{Theta}{See Usage.}
\item{n}{Numeric; combined arithmetically in the body.}
}
\description{
A step of the sarima_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A list with \code{var_theta}, \code{var_Theta}, \code{se_theta}, \code{se_Theta}, \code{cov}, \code{off_diagonal_term}.
}
\keyword{internal}
\examples{
large_sample_se(theta = 0.5, Theta = 0.5, n = 5L)
}
