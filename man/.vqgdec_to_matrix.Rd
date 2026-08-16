% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.vqgdec_to_matrix}
\alias{.vqgdec_to_matrix}
\title{Private helper: coerce a codebook or image (list-of-vectors or}
\usage{
.vqgdec_to_matrix(x)
}
\arguments{
\item{x}{A matrix; passed to \code{dim}.}
}
\description{
matrix) to a 2-D numeric matrix with one row per entry.
}
\value{
A matrix, from \code{as.matrix}.
}
\references{
----------
Esser, P., Rombach, R. & Ommer, B. (2021) "Taming Transformers for
High-Resolution Image Synthesis", CVPR 2021, 12873-12883,
arXiv:2012.09841. Sec. 3.1 ("Learning a Perceptually Rich Codebook":
replacing the L2 reconstruction loss with a perceptual loss and
introducing adversarial training with a PATCH-BASED discriminator to
keep perceptual quality at increased compression), Sec. 3.2 (the
adaptive weight lambda = grad_GL[L_rec] / (grad_GL[L_GAN] + delta)
computed with respect to the last layer L of the decoder), and Sec. 3.3
(sliding-window generation, valid when the dataset statistics are
approximately spatially invariant or spatial conditioning is
available, with conditioning on image coordinates as the remedy
otherwise).

Isola, P., Zhu, J.-Y., Zhou, T. & Efros, A. A. (2017) "Image-to-Image
Translation with Conditional Adversarial Networks", CVPR 2017,
1125-1134, arXiv:1611.07004. The patch-based discriminator.

Zhang, R., Isola, P., Efros, A. A., Shechtman, E. & Wang, O. (2018)
"The Unreasonable Effectiveness of Deep Features as a Perceptual
Metric", CVPR 2018, 586-595, arXiv:1801.03924. The perceptual loss.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.vqgdec_to_matrix(V)
}
