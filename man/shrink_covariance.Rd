% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{shrink_covariance}
\alias{shrink_covariance}
\title{shrink_covariance}
\usage{
shrink_covariance(residuals, lam = NULL)
}
\arguments{
\item{residuals}{A matrix; passed to \code{nrow}.}
\item{lam}{Optional; may be \code{NULL}. Numeric; combined arithmetically in the body.}
}
\description{
A step of the hierF_native implementation. Called by \code{mint_P}. See the file header for the source the module follows.
}
\value{
A list with \code{cov}, \code{lambda}.
}
\keyword{internal}
\examples{
M <- matrix(c(1, 2, 3, 4, 5, 6), nrow = 2)
shrink_covariance(M)
}
