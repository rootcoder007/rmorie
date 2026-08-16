% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.tf_emd}
\alias{.tf_emd}
\title{.tf_emd}
\usage{
.tf_emd(x, maxmodes = 10L, tol = 0.05)
}
\arguments{
\item{x}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{maxmodes}{Coerced to integer by the body, with \code{as.integer}. Defaults to \code{10L}.}
\item{tol}{Passed to \code{.tf_sift}. Defaults to \code{0.05}.}
}
\description{
A step of the rangayyan_tf implementation. Called by \code{EmdEns}, \code{EmdSpec}, \code{Sift} and 2 others in the module. See the file header for the source the module follows.
}
\value{
A list with \code{imfs}, \code{residual}.
}
\keyword{internal}
