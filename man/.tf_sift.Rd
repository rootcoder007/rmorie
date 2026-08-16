% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.tf_sift}
\alias{.tf_sift}
\title{.tf_sift}
\usage{
.tf_sift(x, maxiter = 50L, tol = 0.05)
}
\arguments{
\item{x}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{maxiter}{A count; the body uses it as \code{seq_len(...)}. Defaults to \code{50L}.}
\item{tol}{Defaults to \code{0.05}.}
}
\description{
A step of the rangayyan_tf implementation. Called by \code{.tf_emd}, \code{Imf}. See the file header for the source the module follows.
}
\value{
A list with \code{imf}, \code{iterations}, \code{converged}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.tf_sift(V)
}
