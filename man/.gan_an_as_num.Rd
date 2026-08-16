% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.gan_an_as_num}
\alias{.gan_an_as_num}
\title{Private helpers for the gan_an module}
\usage{
.gan_an_as_num(x)
}
\arguments{
\item{x}{See Usage.}
}
\description{
A step of the gan_an_native implementation. Called by \code{.gan_an_discrimination_loss_impl}, \code{.gan_an_residual_loss_impl}, \code{morie_gan_an} and 2 others in the module. See the file header for the source the module follows.
}
\value{
A vector, from \code{as.numeric}.
}
\references{
----------
Schlegl, T., Seebock, P., Waldstein, S. M., Schmidt-Erfurth, U. &
Langs, G. (2017) "Unsupervised Anomaly Detection with Generative
Adversarial Networks to Guide Marker Discovery", *Information
Processing in Medical Imaging (IPMI 2017)*, LNCS 10265, 146-157,
doi:10.1007/978-3-319-59050-9_12, arXiv:1703.05921. Training a GAN on
normal data to learn a manifold of normal anatomical variability;
mapping a query image back to the latent space by iterative
optimisation of z with the generator fixed; the anomaly score
combining a residual loss on pixel differences with a discrimination
loss on intermediate discriminator features; and the residual image
localising anomalies.

Goodfellow, I. J., Pouget-Abadie, J., Mirza, M., Xu, B.,
Warde-Farley, D., Ozair, S., Courville, A. & Bengio, Y. (2014)
"Generative Adversarial Nets", *NIPS 2014*, 2672-2680,
arXiv:1406.2661.

Radford, A., Metz, L. & Chintala, S. (2016) "Unsupervised
Representation Learning with Deep Convolutional Generative Adversarial
Networks", *ICLR 2016*, arXiv:1511.06434. The DCGAN architecture used.
}
\keyword{internal}
