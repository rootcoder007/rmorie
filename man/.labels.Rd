% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.labels}
\alias{.labels}
\title{.labels}
\usage{
.labels(v, name)
}
\arguments{
\item{v}{Coerced to character by the body, with \code{as.character}.}
\item{name}{Passed to \code{paste0}.}
}
\description{
A step of the randIE_native implementation. Called by \code{morie_randIE_interventional_mean}, \code{morie_randIE_mediator_distribution}. See the file header for the source the module follows.
}
\value{
The value of \code{out}, as built in the body.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.labels(V, V)
}
