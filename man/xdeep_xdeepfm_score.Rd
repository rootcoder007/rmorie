% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{xdeep_xdeepfm_score}
\alias{xdeep_xdeepfm_score}
\title{xdeep_xdeepfm_score}
\usage{
xdeep_xdeepfm_score(x_linear, w_linear, X0, Ws, w_cin, dnn_output = 0, w_dnn = 1, bias = 0)
}
\arguments{
\item{x_linear}{See Usage.}
\item{w_linear}{See Usage.}
\item{X0}{See Usage.}
\item{Ws}{See Usage.}
\item{w_cin}{See Usage.}
\item{dnn_output}{Defaults to \code{0}.}
\item{w_dnn}{Defaults to \code{1}.}
\item{bias}{Defaults to \code{0}.}
}
\description{
Part of the xdeep_native implementation; see the file header for the source it follows.
}
\value{
A list with \code{logit}, \code{probability}, \code{linear}, \code{cin}, \code{dnn}, \code{note}.
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
