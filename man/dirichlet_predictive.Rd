% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{dirichlet_predictive}
\alias{dirichlet_predictive}
\title{dirichlet_predictive}
\usage{
dirichlet_predictive(N_ij, N_j, beta, m)
}
\arguments{
\item{N_ij}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{N_j}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{beta}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{m}{Coerced to numeric by the body, with \code{as.numeric}.}
}
\description{
A step of the bigtm_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A list with \code{predictive}, \code{lambda}, \code{f}, \code{interpolated}, \code{note}.
}
\keyword{internal}
\examples{
dirichlet_predictive(N_ij = c(3, 5, 2), N_j = 10, beta = 0.5,
                     m = c(0.2, 0.3, 0.5))
}
