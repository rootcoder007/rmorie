% Generated from the module source by ledger/wave3/gen_rd_any.R --
% roxygen2 hangs on this package. Usage comes from args(), so it
% tracks the formals.
\name{morie_vit2lf}
\alias{morie_vit2lf}
\title{Attention with the logits scaled by log n, by a cosine, or by both}
\usage{
morie_vit2lf(q, k, v, mode = "logn", tau = 1, bias = NULL, mask = NULL, n = NULL, 
    tau_floor = .VIT2LF_TAU_FLOOR) 
}
\arguments{
\item{q}{Queries, one row per query position.}
\item{k}{Keys, one row per key position, sharing the head dimension.}
\item{v}{Values, one row per key position.}
\item{mode}{A member of the mode list.}
\item{tau}{The cosine temperature, held at or above the floor.}
\item{bias}{An additive position bias, applied after the scaling, or   NULL.}
\item{mask}{A logical matrix, TRUE where a key is visible to a query,   or NULL.}
\item{n}{The length used for the log scaling, or NULL for the number   of keys.}
\item{tau_floor}{The lower bound on tau.}
}
\description{
Attention with the logits scaled by log n, by a cosine, or by both
}
\value{
A list with the attention weights, the context vectors, the   logits, the scale actually applied, the per-row entropy and the   largest weight -- the last two being how you see the scaling   working.
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
morie_vit2lf(q = 0.5, k = c(1, 2, 3, 4, 5, 6, 7, 8), v = c(1, 2, 3, 4, 5, 6, 7, 8))
}
