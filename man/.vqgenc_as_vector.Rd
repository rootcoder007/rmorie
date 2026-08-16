% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.vqgenc_as_vector}
\alias{.vqgenc_as_vector}
\title{.vqgenc_as_vector}
\usage{
.vqgenc_as_vector(x)
}
\arguments{
\item{x}{See Usage.}
}
\description{
Part of the vqgenc_native implementation; see the file header for the source it follows.
}
\value{
Nothing; this branch always raises.
}
\references{
----------
Esser, P., Rombach, R. & Ommer, B. (2021) "Taming Transformers for
High-Resolution Image Synthesis", *Proceedings of the IEEE/CVF
Conference on Computer Vision and Pattern Recognition (CVPR 2021)*,
12873-12883, arXiv:2012.09841. Sec. 3.1: transformers containing no
inductive prior on locality and therefore having to learn all
relationships, with quadratically increasing cost; the use of a
convolutional approach to learn a codebook of context-rich visual
parts and a transformer to model their global composition; the
nearest-code quantisation s_ij = k such that (z_q)_ij = z_k; the
straight-through gradient estimator copying gradients from the decoder
to the encoder so model and codebook train end to end; and the loss
L_VQ = ||x - x_hat||^2 + ||sg[E(x)] - z_q||^2 + ||sg[z_q] - E(x)||^2
whose last term is the commitment loss.

van den Oord, A., Vinyals, O. & Kavukcuoglu, K. (2017) "Neural
Discrete Representation Learning", *NIPS 2017*, 6306-6315,
arXiv:1711.00937. VQ-VAE, the commitment loss and the
straight-through estimator this builds on.
"""
}
\keyword{internal}
