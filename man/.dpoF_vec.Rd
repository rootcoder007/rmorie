% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.dpoF_vec}
\alias{.dpoF_vec}
\title{.dpoF_vec}
\usage{
.dpoF_vec(x, name)
}
\arguments{
\item{x}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{name}{See Usage.}
}
\description{
A step of the dpoF_native implementation. Called by \code{morie_dpoF}, \code{optimal_policy}. See the file header for the source the module follows.
}
\value{
The value of \code{v}, as built in the body.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.dpoF_vec(V, V)
}
