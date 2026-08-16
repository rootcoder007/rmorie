% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.w3_chol}
\alias{.w3_chol}
\title{Lower Cholesky factor L with A = L L\\'. Explicit, not chol()}
\usage{
.w3_chol(a)
}
\arguments{
\item{a}{A matrix; indexed by row and column.}
}
\description{
A step of the helpers_w3num implementation. Called by \code{.w3_ols}, \code{morie_cypin_fit}, \code{morie_hyper2} and 3 others in the module. See the file header for the source the module follows.
}
\value{
The value of \code{lo}, as built in the body.
}
\keyword{internal}
