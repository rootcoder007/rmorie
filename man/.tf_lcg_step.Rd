% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.tf_lcg_step}
\alias{.tf_lcg_step}
\title{.tf_lcg_step}
\usage{
.tf_lcg_step(st)
}
\arguments{
\item{st}{A vector; indexed elementwise.}
}
\description{
A step of the rangayyan_tf implementation. Called by \code{.tf_lcg_seed}, \code{EmdEns}. See the file header for the source the module follows.
}
\value{
The value of \code{r}, as built in the body.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.tf_lcg_step(V)
}
