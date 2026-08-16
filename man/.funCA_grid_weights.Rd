% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.funCA_grid_weights}
\alias{.funCA_grid_weights}
\title{.funCA_grid_weights}
\usage{
.funCA_grid_weights(n_t)
}
\arguments{
\item{n_t}{A count; the body uses it as \code{rep(...)}.}
}
\description{
A step of the funCA_native implementation. Called by \code{morie_funCA_functional_cca}. See the file header for the source the module follows.
}
\value{
The value of \code{w}, as built in the body.
}
\keyword{internal}
\examples{
.funCA_grid_weights(n_t = 5L)
}
