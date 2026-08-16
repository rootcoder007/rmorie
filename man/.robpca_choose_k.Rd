% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.robpca_choose_k}
\alias{.robpca_choose_k}
\title{.robpca_choose_k}
\usage{
.robpca_choose_k(l0, k, kmax, r1)
}
\arguments{
\item{l0}{A vector; indexed elementwise.}
\item{k}{Optional; may be \code{NULL}. A vector; its length is taken.}
\item{kmax}{Numeric; passed to \code{min}.}
\item{r1}{Numeric; passed to \code{min}.}
}
\description{
A step of the robpca_native implementation. Called by \code{morie_robpca}. See the file header for the source the module follows.
}
\value{
Nothing; this branch always raises.
}
\keyword{internal}
\examples{
.robpca_choose_k(l0 = c(1, 2, 3, 4, 5, 6, 7, 8), k = 5L, kmax = c(1, 2, 3, 4, 5, 6, 7, 8), r1 = c(1, 2, 3, 4, 5, 6, 7, 8))
}
