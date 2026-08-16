% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{xdeep_cin}
\alias{xdeep_cin}
\title{xdeep_cin}
\usage{
xdeep_cin(X0, Ws)
}
\arguments{
\item{X0}{Passed to \code{.xdeep_to_mat}.}
\item{Ws}{A vector; its length is taken.}
}
\description{
A step of the xdeep_native implementation. Called by \code{xdeep_xdeepfm_score}. See the file header for the source the module follows.
}
\value{
A list with \code{estimate}, \code{pooled}, \code{layers}, \code{degrees}, \code{n_layers}, \code{method}, \code{note}.
}
\references{
----------
Lian, J., Zhou, X., Zhang, F., Chen, Z., Xie, X. & Sun, G. (2018)
"xDeepFM: Combining Explicit and Implicit Feature Interactions for
Recommender Systems", *Proceedings of the 24th ACM SIGKDD
International Conference on Knowledge Discovery and Data Mining (KDD
\'18)*, 1754-1763, doi:10.1145/3219819.3220023, arXiv:1803.05170.
Rendle, S. (2010) "Factorization Machines", *ICDM 2010*, 995-1000,
doi:10.1109/ICDM.2010.127. The vector-wise tradition; implemented in
:mod:`fmFM`.
Guo, H., Tang, R., Ye, Y., Li, Z. & He, X. (2017) "DeepFM: A
Factorization-Machine based Neural Network for CTR Prediction",
*IJCAI 2017*, 1725-1731, doi:10.24963/ijcai.2017/239.
}
\keyword{internal}
\examples{
xdeep_cin(X0 = 5L, Ws = c(1, 2, 3, 4, 5, 6, 7, 8))
}
