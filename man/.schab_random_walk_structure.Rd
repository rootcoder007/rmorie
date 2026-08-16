% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.schab_random_walk_structure}
\alias{.schab_random_walk_structure}
\title{.schab_random_walk_structure}
\usage{
.schab_random_walk_structure(n_time, order = 1L)
}
\arguments{
\item{n_time}{Coerced to integer by the body, with \code{as.integer}.}
\item{order}{Coerced to integer by the body, with \code{as.integer}. Defaults to \code{1L}.}
}
\description{
A step of the schab_glmm_shared implementation. Called by \code{spbayr}. See the file header for the source the module follows.
}
\value{
The value of \code{\%*\%}.
}
\keyword{internal}
\examples{
.schab_random_walk_structure(n_time = 5L)
}
