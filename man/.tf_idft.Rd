% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.tf_idft}
\alias{.tf_idft}
\title{.tf_idft}
\usage{
.tf_idft(X)
}
\arguments{
\item{X}{Coerced to complex by the body, with \code{as.complex}.}
}
\description{
A step of the rangayyan_tf implementation. Called by \code{.tf_analytic}, \code{CDemod}, \code{CprWt} and 2 others in the module. See the file header for the source the module follows.
}
\value{
A vector, from \code{vapply}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.tf_idft(V)
}
