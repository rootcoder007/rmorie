% Generated from the module source by ledger/wave3/gen_rd_any.R --
% roxygen2 hangs on this package. Usage comes from args(), so it
% tracks the formals.
\name{morie_vit2lf_log_coords}
\alias{morie_vit2lf_log_coords}
\title{Swin V2 log-spaced relative coordinates}
\usage{
morie_vit2lf_log_coords(dx, dy)
}
\arguments{
\item{dx}{Horizontal offset.}
\item{dy}{Vertical offset.}
}
\description{
sign(d) log(1 + |d|), which is odd, zero at zero, and compresses the range so a bias learnt at one window size extrapolates to another. On an eight-by-eight window the raw range [-7, 7] becomes [-2.079, 2.079], the figure the paper quotes.
}
\value{
The two log-spaced offsets.
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
