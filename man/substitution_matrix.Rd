% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{substitution_matrix}
\alias{substitution_matrix}
\title{substitution_matrix}
\usage{
substitution_matrix(t, pi = NULL, u = 1) 
}
\arguments{
\item{t}{Numeric; combined arithmetically in the body.}
\item{pi}{Passed to \code{.phylml_pi}.}
\item{u}{Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{1}.}
}
\description{
A step of the phylml_native implementation. Called by \code{.phylml_prune}. See the file header for the source the module follows.
}
\value{
The value of \code{M}, as built in the body.
}
\keyword{internal}
\examples{
substitution_matrix(t = 5L)
}
