% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.robpca_fast_mcd}
\alias{.robpca_fast_mcd}
\title{.robpca_fast_mcd}
\usage{
.robpca_fast_mcd(rows, h, n_start = 250L, seed = 17L)
}
\arguments{
\item{rows}{A matrix; indexed by row and column.}
\item{h}{See Usage.}
\item{n_start}{Coerced to integer by the body, with \code{as.integer}. Defaults to \code{250L}.}
\item{seed}{Numeric; combined arithmetically in the body. Defaults to \code{17L}.}
}
\description{
A step of the robpca_native implementation. Called by \code{morie_robpca}. See the file header for the source the module follows.
}
\value{
The value of \code{best}, as built in the body.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.robpca_fast_mcd(V, V)
}
