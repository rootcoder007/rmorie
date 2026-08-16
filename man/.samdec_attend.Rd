% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.samdec_attend}
\alias{.samdec_attend}
\title{.samdec_attend}
\usage{
.samdec_attend(Q, K, V)
}
\arguments{
\item{Q}{A matrix; indexed by row and column.}
\item{K}{A matrix; passed to \code{nrow}.}
\item{V}{A matrix; passed to \code{crossprod}.}
}
\description{
A step of the samdec_native implementation. Called by \code{two_way_block}. See the file header for the source the module follows.
}
\value{
A list with \code{out}, \code{W}.
}
\keyword{internal}
\examples{
M <- matrix(c(1, 2, 3, 4, 5, 6), nrow = 2)
S <- c("a", "b", "c")
dynamic_mask_head(S, M)
.samdec_attend(Q = M, K = M, V = M)
}
