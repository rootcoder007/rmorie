% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_sv_cmdscale}
\alias{.morie_sv_cmdscale}
\title{Internal helper: classical multidimensional scaling}
\usage{
.morie_sv_cmdscale(D, k = 2L)
}
\arguments{
\item{D}{Distance matrix (m by m), symmetric with a zero diagonal.}
\item{k}{Number of dimensions to return.}
}
\description{
Torgerson-Gower classical scaling. Double-centre the squared distance matrix to recover the Gram matrix, then take its leading eigenvectors:   B = -1/2 J D^2 J,  J = I - (1/m) 1 1\'   B = V L V\',  X = V_k L_k^(1/2) Native replacement for `stats::cmdscale`. Non-positive eigenvalues are clamped to zero, which is what happens whenever D is not Euclidean.
}
\value{
An m by k matrix of coordinates, columns ordered by decreasing   eigenvalue.
}
\keyword{internal}
\examples{
Xt <- matrix(rnorm(20), 10, 2)
Dt <- as.matrix(stats::dist(Xt))
.morie_sv_bayes_mds(Dt, n_dims = 2L, n_samples = 200L, burn_in = 100L)
.morie_sv_cmdscale(D = Dt)
}
