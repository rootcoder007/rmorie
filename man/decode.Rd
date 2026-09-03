% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{decode}
\alias{decode}
\title{decode}
\usage{
decode(z, Wp, bp, items = NULL, activation = "sigmoid") 
}
\arguments{
\item{z}{Numeric; combined arithmetically in the body.}
\item{Wp}{A vector; indexed elementwise.}
\item{bp}{A vector; its length is taken and its elements indexed.}
\item{items}{Optional; may be \code{NULL}. Coerced to integer by the body, with \code{as.integer}.}
\item{activation}{Passed to \code{.cdae_act}. Defaults to \code{"sigmoid"}.}
}
\description{
A step of the cdaeRC_native implementation. Called by \code{fit_cdae}, \code{recommend}. See the file header for the source the module follows.
}
\value{
A vector, from \code{sapply}.
}
\keyword{internal}
\examples{
decode(z = c(1, 2, 3, 4, 5, 6, 7, 8), Wp = c(1, 2, 3, 4, 5, 6, 7, 8),
  bp = c(1, 2, 3, 4, 5, 6, 7, 8))
}
