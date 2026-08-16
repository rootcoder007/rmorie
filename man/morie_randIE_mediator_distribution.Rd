% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_randIE_mediator_distribution}
\alias{morie_randIE_mediator_distribution}
\title{morie_randIE_mediator_distribution}
\usage{
morie_randIE_mediator_distribution(A, M, C = NULL, laplace = 0)
}
\arguments{
\item{A}{Passed to \code{.labels}.}
\item{M}{Passed to \code{.labels}.}
\item{C}{Optional; may be \code{NULL}. Passed to \code{.labels}.}
\item{laplace}{Numeric; combined arithmetically in the body. Defaults to \code{0}.}
}
\description{
A step of the randIE_native implementation. Called by \code{morie_randIE_interventional_mean}. See the file header for the source the module follows.
}
\value{
A list with \code{p}, \code{levels}, \code{strata}, \code{arms}, \code{n}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
morie_randIE_mediator_distribution(V, V)
}
