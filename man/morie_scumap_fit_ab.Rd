% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_scumap_fit_ab}
\alias{morie_scumap_fit_ab}
\title{morie_scumap_fit_ab}
\usage{
morie_scumap_fit_ab(min_dist = 0.1, spread = 1, n_grid = 300, iters = 200)
}
\arguments{
\item{min_dist}{Numeric; combined arithmetically in the body. Defaults to \code{0.1}.}
\item{spread}{Numeric; combined arithmetically in the body. Defaults to \code{1}.}
\item{n_grid}{A count; the body uses it as \code{seq_len(...)}. Defaults to \code{300}.}
\item{iters}{Defaults to \code{200}.}
}
\description{
A step of the scumap_native implementation. Called by \code{morie_scumap_umap_singlecell}. See the file header for the source the module follows.
}
\value{
A list with \code{a}, \code{b}.
}
\keyword{internal}
\examples{
morie_scumap_fit_ab()
}
