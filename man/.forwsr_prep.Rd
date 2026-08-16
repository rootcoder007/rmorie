% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.forwsr_prep}
\alias{.forwsr_prep}
\title{.forwsr_prep}
\usage{
.forwsr_prep(X, y)
}
\arguments{
\item{X}{A matrix; passed to \code{as.matrix}.}
\item{y}{Coerced to numeric by the body, with \code{as.numeric}.}
}
\description{
A step of the forwsr_native implementation. Called by \code{morie_forwsr}, \code{morie_forwsr_forward_search}, \code{morie_forwsr_lms_start} and 1 others in the module. See the file header for the source the module follows.
}
\value{
A list with \code{M}, \code{y}, \code{n}, \code{p}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.forwsr_prep(V, V)
}
