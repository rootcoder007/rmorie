% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.schab_initial_mu}
\alias{.schab_initial_mu}
\title{.schab_initial_mu}
\usage{
.schab_initial_mu(z, family)
}
\arguments{
\item{z}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{family}{Passed to \code{identical}.}
}
\description{
A step of the schab_glmm_shared implementation. Called by \code{.schab_fit_pseudo_likelihood}. See the file header for the source the module follows.
}
\value{
The value of \code{z}, as built in the body.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.schab_initial_mu(V, V)
}
