% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_scm_solve}
\alias{.morie_scm_solve}
\title{.morie_scm_solve}
\usage{
.morie_scm_solve(u, equations)
}
\arguments{
\item{u}{Coerced to list by the body, with \code{as.list}.}
\item{equations}{A vector; indexed elementwise.}
}
\description{
A step of the counterfactual implementation. Called by \code{Counterfactual}. See the file header for the source the module follows.
}
\value{
The value of \code{vals}, as built in the body.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.morie_scm_solve(V, V)
}
