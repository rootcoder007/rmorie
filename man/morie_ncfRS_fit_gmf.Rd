% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_ncfRS_fit_gmf}
\alias{morie_ncfRS_fit_gmf}
\title{morie_ncfRS_fit_gmf}
\usage{
morie_ncfRS_fit_gmf(pos, n_users, n_items, k_dim = 8, alpha = 0.05, iters = 2000, n_neg = 4, seed = 0, learn_h = TRUE)
}
\arguments{
\item{pos}{See Usage.}
\item{n_users}{See Usage.}
\item{n_items}{See Usage.}
\item{k_dim}{Defaults to \code{8}.}
\item{alpha}{Defaults to \code{0.05}.}
\item{iters}{Defaults to \code{2000}.}
\item{n_neg}{Defaults to \code{4}.}
\item{seed}{Defaults to \code{0}.}
\item{learn_h}{Defaults to \code{TRUE}.}
}
\description{
Part of the ncfRS_native implementation; see the file header for the source it follows.
}
\value{
A list with \code{estimate}, \code{P}, \code{Q}, \code{h}, \code{loss_history}, \code{final_loss}, \code{k}, \code{learned_h}, \code{method}.
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
