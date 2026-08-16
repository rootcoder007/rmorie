% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.schab_data_covariance}
\alias{.schab_data_covariance}
\title{Sigma^2 V^1/2 R V^1/2, on the DATA scale. Sec. 6.3.5.3 writes its}
\usage{
.schab_data_covariance(mu, sigma2, family, R = NULL)
}
\arguments{
\item{mu}{See Usage.}
\item{sigma2}{See Usage.}
\item{family}{See Usage.}
\item{R}{Defaults to \code{NULL}.}
}
\description{
score equations with the symbol Sigma_mu, but the matrix they need is this one; with (6.79) as written the scores are wrong by a factor of Psi^2.
}
\value{
A numeric value.
}
\keyword{internal}
