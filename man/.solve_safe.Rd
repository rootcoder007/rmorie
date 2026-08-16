% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.solve_safe}
\alias{.solve_safe}
\title{Solve a linear system}
\usage{
.solve_safe(A, b)
}
\arguments{
\item{A}{A matrix; passed to \code{solve}.}
\item{b}{A matrix; passed to \code{solve}.}
}
\description{
A step of the andmnh_native implementation. Called by \code{prewhiten_var}. See the file header for the source the module follows.
}
\value{
A matrix, from \code{solve}.
}
\keyword{internal}
