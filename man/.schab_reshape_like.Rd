% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.schab_reshape_like}
\alias{.schab_reshape_like}
\title{.schab_reshape_like}
\usage{
.schab_reshape_like(values, template)
}
\arguments{
\item{values}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{template}{A matrix; passed to \code{dim}.}
}
\description{
A step of the schab_gwr_shared implementation. Called by \code{.schab_kernel_weights}. See the file header for the source the module follows.
}
\value{
The value of \code{array}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.schab_reshape_like(V, V)
}
