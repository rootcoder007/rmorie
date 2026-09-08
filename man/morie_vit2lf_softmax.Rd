% Generated from the module source by ledger/wave3/gen_rd_any.R --
% roxygen2 hangs on this package. Usage comes from args(), so it
% tracks the formals.
\name{morie_vit2lf_softmax}
\alias{morie_vit2lf_softmax}
\title{Row-wise softmax, max-shifted, with a compensated denominator}
\usage{
morie_vit2lf_softmax(logits, mask = NULL) 
}
\arguments{
\item{logits}{The logit matrix.}
\item{mask}{A logical matrix, TRUE where a key is visible, or NULL.}
}
\description{
Masked entries are removed before the shift rather than pushed to a large negative number and exponentiated, so a fully masked row is an error instead of a silently uniform one.
}
\value{
The attention weight matrix.
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
M <- matrix(c(1, 2, 3, 4, 5, 6), nrow = 2)
morie_vit2lf_softmax(M)
}
