% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.funCA_fpca}
\alias{.funCA_fpca}
\title{.funCA_fpca}
\usage{
.funCA_fpca(C, w, n_keep)
}
\arguments{
\item{C}{Numeric; combined arithmetically in the body.}
\item{w}{A vector; its length is taken.}
\item{n_keep}{A count; the body uses it as \code{seq_len(...)}.}
}
\description{
A step of the funCA_native implementation. Called by \code{morie_funCA_functional_cca}. See the file header for the source the module follows.
}
\value{
A list with \code{lam}, \code{phi}, \code{all}.
}
\keyword{internal}
\examples{
.funCA_fpca(C = c(1, 2, 3, 4, 5, 6, 7, 8), w = c(1, 2, 3, 4, 5, 6, 7, 8), n_keep = c(1, 2, 3, 4, 5, 6, 7, 8))
}
