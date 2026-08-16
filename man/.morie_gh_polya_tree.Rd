% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_gh_polya_tree}
\alias{.morie_gh_polya_tree}
\title{.morie_gh_polya_tree}
\usage{
.morie_gh_polya_tree(x, grid, levels = 6L, a_fn = NULL, lo = NULL, hi = NULL)
}
\arguments{
\item{x}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{grid}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{levels}{Coerced to integer by the body, with \code{as.integer}. Defaults to \code{6L}.}
\item{a_fn}{Optional; may be \code{NULL}. Passed to \code{is.null}.}
\item{lo}{Optional; may be \code{NULL}. Coerced to numeric by the body, with \code{as.numeric}.}
\item{hi}{Optional; may be \code{NULL}. Coerced to numeric by the body, with \code{as.numeric}.}
}
\description{
A step of the ghosal_native implementation. Called by \code{morie_polya_tree_density}, \code{morie_polya_tree_mixture}. See the file header for the source the module follows.
}
\value{
The value of \code{ifelse}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.morie_gh_polya_tree(V, V)
}
