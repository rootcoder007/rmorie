% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{lda_predictive}
\alias{lda_predictive}
\title{lda_predictive}
\usage{
lda_predictive(N_ik, N_k, beta, m) 
}
\arguments{
\item{N_ik}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{N_k}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{beta}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{m}{Coerced to numeric by the body, with \code{as.numeric}.}
}
\description{
A step of the bigtm_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A list with \code{predictive}, \code{lambda}, \code{eq15_as_printed}, \code{note}.
}
\keyword{internal}
\examples{
lda_predictive(N_ik = c(1, 2, 3, 4, 5, 6, 7, 8), N_k = 5L, beta = 0.5, m = 5L)
}
