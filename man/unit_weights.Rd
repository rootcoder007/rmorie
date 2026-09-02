% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{unit_weights}
\alias{unit_weights}
\title{unit_weights}
\usage{
unit_weights(Y, treated, t_post, zeta = NULL) 
}
\arguments{
\item{Y}{Passed to \code{.causscd_grid}.}
\item{treated}{Passed to \code{.causscd_grid}.}
\item{t_post}{A count; the body uses it as \code{seq_len(...)}.}
\item{zeta}{Optional; may be \code{NULL}. Numeric; combined arithmetically in the body.}
}
\description{
A step of the causscd_native implementation. Called by \code{sdid}. See the file header for the source the module follows.
}
\value{
A list with \code{weights}, \code{intercept}, \code{zeta}.
}
\keyword{internal}
\examples{
set.seed(1)
Y <- lapply(1:8, function(i) rnorm(10, 10))
for (i in 7:8) Y[[i]][7:10] <- Y[[i]][7:10] + 3
treated <- c(rep(0L, 6), 1L, 1L)
str(unit_weights(Y, treated, t_post = 6), max.level = 1)
}
