% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{gaussian_expansion}
\alias{gaussian_expansion}
\title{gaussian_expansion}
\usage{
gaussian_expansion(r, mu_min = 0, mu_max = 6, n_gaussians = 25, gamma = NULL) 
}
\arguments{
\item{r}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{mu_min}{Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{0}.}
\item{mu_max}{Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{6}.}
\item{n_gaussians}{Coerced to integer by the body, with \code{as.integer}. Defaults to \code{25}.}
\item{gamma}{Optional; may be \code{NULL}. Coerced to numeric by the body, with \code{as.numeric}.}
}
\description{
A step of the schN_native implementation. Called by \code{cfconv}. See the file header for the source the module follows.
}
\value{
A numeric value.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
gaussian_expansion(V)
}
