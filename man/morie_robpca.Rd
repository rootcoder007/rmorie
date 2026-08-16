% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_robpca}
\alias{morie_robpca}
\title{morie_robpca}
\usage{
morie_robpca(X, k = NULL, alpha = 0.75, kmax = 10L, n_dirs = 250L, n_start = 250L, seed = 17L, reweight = TRUE)
}
\arguments{
\item{X}{See Usage.}
\item{k}{Defaults to \code{NULL}.}
\item{alpha}{Defaults to \code{0.75}.}
\item{kmax}{Defaults to \code{10L}.}
\item{n_dirs}{Defaults to \code{250L}.}
\item{n_start}{Defaults to \code{250L}.}
\item{seed}{Defaults to \code{17L}.}
\item{reweight}{Defaults to \code{TRUE}.}
}
\description{
Part of the robpca_native implementation; see the file header for the source it follows.
}
\value{
A list with \code{estimate}, \code{loadings}, \code{eigenvalues}, \code{center}, \code{scores}, \code{k}, \code{k0}, \code{h}, \code{alpha}, \code{rank}, \code{subspace_rank}, \code{outlyingness}, \code{h_subset}, \code{reweighted_kept}, \code{consistency_factor}, \code{score_distance}, \code{orthogonal_distance}, \code{sd_cutoff}, \code{od_cutoff}, \code{classification}, \code{n_outliers}, \code{n}, \code{p}, \code{reweighted}, \code{method}, \code{note}.
}
\keyword{internal}
\examples{
D <- data.frame(x = c(1, 2, 3, 4), y = c(2, 4, 5, 9))
morie_robpca(D)
}
