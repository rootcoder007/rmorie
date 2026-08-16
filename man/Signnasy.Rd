% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{Signnasy}
\alias{Signnasy}
\title{Two-sided sign-test sample size -- eq. (5.4.9) with alpha/2, p. 179}
\usage{
Signnasy(theta, alpha = 0.05, beta = 0.1)
}
\description{
Two-sided sign-test sample size -- eq. (5.4.9) with alpha/2, p. 179
}
\value{
A list with \code{n}, \code{n_raw}, \code{root_n}, \code{z_alpha}, \code{z_beta}, \code{theta}.
}
\keyword{internal}
\examples{
Signnasy(theta = 0.05)
}
