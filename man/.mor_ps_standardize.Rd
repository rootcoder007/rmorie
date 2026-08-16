% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.mor_ps_standardize}
\alias{.mor_ps_standardize}
\title{.mor_ps_standardize}
\usage{
.mor_ps_standardize(X)
}
\arguments{
\item{X}{A matrix; indexed by row and column.}
}
\description{
A step of the causal implementation. Called by \code{.fit_propensity}. See the file header for the source the module follows.
}
\value{
The value of \code{X}, as built in the body.
}
\keyword{internal}
\examples{
M <- matrix(c(1, 2, 3, 4, 5, 6), nrow = 2)
.mor_ps_standardize(M)
}
