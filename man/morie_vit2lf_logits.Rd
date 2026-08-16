% Generated from the module source by ledger/wave3/gen_rd_any.R --
% roxygen2 hangs on this package. Usage comes from args(), so it
% tracks the formals.
\name{morie_vit2lf_logits}
\alias{morie_vit2lf_logits}
\title{The pre-softmax attention scores, one row per query}
\usage{
morie_vit2lf_logits(q, k, mode = "dot", tau = 1, bias = NULL, n = NULL, tau_floor = .VIT2LF_TAU_FLOOR)
}
\arguments{
\item{q}{Query matrix, one row per query position.}
\item{k}{Key matrix, one row per key position.}
\item{mode}{A member of the mode list.}
\item{tau}{The cosine temperature.}
\item{bias}{An additive position bias matrix, or NULL.}
\item{n}{The length used for the log scaling, or NULL for the number   of keys.}
\item{tau_floor}{The lower bound on tau.}
}
\description{
"dot" is the inner product over the square root of the head dimension; "logn" is the same multiplied by log n, with n defaulting to the number of KEYS, which is the number of terms the softmax is spreading its mass over; "cosine" is the cosine of the angle over tau, with no square root of d because a cosine is already normalised, which is the whole point of using one; "logn_cosine" is the cosine route with the log-length factor as well. An additive bias is applied after the scaling in every route, as both papers write it -- a bias scaled along with the logits would change meaning with the sequence length, which is exactly what a position bias must not do.
}
\value{
A list with the logit matrix and the scale actually applied.
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
