% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{xdeep_cin_layer}
\alias{xdeep_cin_layer}
\title{xdeep_cin_layer}
\usage{
xdeep_cin_layer(X_prev, X0, W) 
}
\arguments{
\item{X_prev}{Passed to \code{.xdeep_to_mat}.}
\item{X0}{Passed to \code{.xdeep_to_mat}.}
\item{W}{A vector; its length is taken and its elements indexed.}
}
\description{
A step of the xdeep_native implementation. Called by \code{xdeep_cin}. See the file header for the source the module follows.
}
\value{
The value of \code{out}, as built in the body.
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
xdeep_cin_layer(X_prev = 5L, X0 = 5L, W = c(1, 2, 3, 4, 5, 6, 7, 8))
}
