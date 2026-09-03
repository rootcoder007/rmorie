% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{n_parameters}
\alias{n_parameters}
\title{n_parameters}
\usage{
n_parameters(n_features, n_fields, k_dim, model = "ffm") 
}
\arguments{
\item{n_features}{Coerced to integer by the body, with \code{as.integer}.}
\item{n_fields}{Coerced to integer by the body, with \code{as.integer}.}
\item{k_dim}{Coerced to integer by the body, with \code{as.integer}.}
\item{model}{One of \code{"ffm"}, \code{"fm"}. Defaults to \code{"ffm"}.}
}
\description{
A step of the ffmFM_native implementation. Called by \code{fit_ffm}. See the file header for the source the module follows.
}
\value{
One of two values, depending on the branch taken.
}
\keyword{internal}
\examples{
n_parameters(n_features = c(1, 2, 3, 4, 5, 6, 7, 8), n_fields = c(1, 2, 3, 4, 5, 6, 7, 8),
  k_dim = c(1, 2, 3, 4, 5, 6, 7, 8))
}
