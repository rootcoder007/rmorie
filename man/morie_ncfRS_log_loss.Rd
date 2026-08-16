% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_ncfRS_log_loss}
\alias{morie_ncfRS_log_loss}
\title{morie_ncfRS_log_loss}
\usage{
morie_ncfRS_log_loss(y, y_hat)
}
\arguments{
\item{y}{See Usage.}
\item{y_hat}{See Usage.}
}
\description{
A step of the ncfRS_native implementation. Called by \code{morie_ncfRS_fit_gmf}. See the file header for the source the module follows.
}
\value{
A numeric value.
}
\references{
----------
He, X., Liao, L., Zhang, H., Nie, L., Hu, X. & Chua, T.-S. (2017)
"Neural Collaborative Filtering", *Proceedings of the 26th
International Conference on World Wide Web (WWW \'17)*, 173-182,
doi:10.1145/3038912.3052569. Sec. 3.1 (the general NCF framework and
the log loss with sampled negatives for implicit data), Sec. 3.2
(GMF: eq. (9), and the demonstration that MF is recovered when a_out
is the identity and h is uniform), Sec. 3.3 (MLP over the
concatenation), and Sec. 3.4 (NeuMF: separate GMF and MLP embeddings
fused in the last layer, and why sharing one embedding would limit
the fused model).

Koren, Y., Bell, R. & Volinsky, C. (2009) "Matrix Factorization
Techniques for Recommender Systems", *Computer* 42(8), 30-37,
doi:10.1109/MC.2009.263. The inner-product model being generalised.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
morie_ncfRS_log_loss(V, V)
}
