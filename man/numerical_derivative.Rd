% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{numerical_derivative}
\alias{numerical_derivative}
\title{numerical_derivative}
\usage{
numerical_derivative(psi_of_P, weights, score, h = 1e-05) 
}
\arguments{
\item{psi_of_P}{Accepted by the signature and not used anywhere in the body.}
\item{weights}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{score}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{h}{Numeric; combined arithmetically in the body. Defaults to \code{1e-05}.}
}
\description{
A step of the tlheic_native implementation. Called by \code{estimate_eic}, \code{morie_tlheic}, \code{verify_gradient}. See the file header for the source the module follows.
}
\value{
A numeric value.
}
\keyword{internal}
\examples{
set.seed(1)
w <- rep(1 / 20, 20)
score <- rnorm(20)
psi <- function(wv) sum(wv * (1:20))
numerical_derivative(psi, w, score)
}
