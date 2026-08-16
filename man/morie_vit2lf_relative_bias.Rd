% Generated from the module source by ledger/wave3/gen_rd_any.R --
% roxygen2 hangs on this package. Usage comes from args(), so it
% tracks the formals.
\name{morie_vit2lf_relative_bias}
\alias{morie_vit2lf_relative_bias}
\title{Look a relative position up in a bias table}
\usage{
morie_vit2lf_relative_bias(coords, table, window, log_spaced = TRUE)
}
\arguments{
\item{coords}{A two-column matrix of positions.}
\item{table}{A (2 window - 1) square bias table.}
\item{window}{The window size.}
\item{log_spaced}{Whether to transform the offsets first.}
}
\description{
With log spacing the offsets are transformed first and the table is read at the nearest tabulated log-spaced offset, which is what the meta-network approximates continuously.
}
\value{
The bias matrix.
}
\references{
  Chiang, D. and Cholak, P. (2022) "Overcoming a theoretical
    limitation of self-attention." Proceedings of the 60th Annual
    Meeting of the Association for Computational Linguistics
    (Volume 1: Long Papers), 7654-7664, Dublin. arXiv:2202.12172.
    Their equation (2) and section 5.3.
  Liu, Z., Hu, H., Lin, Y., Yao, Z., Xie, Z., Wei, Y., Ning, J., Cao,
    Y., Zhang, Z., Dong, L., Wei, F. and Guo, B. (2022) "Swin
    Transformer V2: scaling up capacity and resolution." Proceedings
    of the IEEE/CVF Conference on Computer Vision and Pattern
    Recognition (CVPR), 12009-12019. arXiv:2111.09883. Their
    equations (2) and (4).
  Vaswani, A. et al. (2017) "Attention is all you need." Advances in
    Neural Information Processing Systems 30.
  Hahn, M. (2020) "Theoretical limitations of self-attention in neural
    sequence models." Transactions of the ACL 8, 156-171.
}
\keyword{internal}
\examples{
mk <- function(nr, nc, f) {
    m <- matrix(0, nr, nc)
    for (i in seq_len(nr)) for (j in seq_len(nc)) m[i, j] <- f(i - 
        1L, j - 1L)
    m
}
W <- 4L
SPAN <- 2L * W - 1L
TABLE <- mk(SPAN, SPAN, function(a, b) (((a * 3 + b * 5)\%\%9) - 
    4)/6)
COORDS <- matrix(0, 6L, 2L)
morie_vit2lf_relative_bias(COORDS, TABLE, W, TRUE)
}
