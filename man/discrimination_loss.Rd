% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{discrimination_loss}
\alias{discrimination_loss}
\title{discrimination_loss}
\usage{
discrimination_loss(f_x, f_gz) 
}
\arguments{
\item{f_x}{Passed to \code{.gan_an_discrimination_loss_impl}.}
\item{f_gz}{Passed to \code{.gan_an_discrimination_loss_impl}.}
}
\description{
A step of the gan_an_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
The value of \code{.gan_an_discrimination_loss_impl}.
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
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
discrimination_loss(V, V)
}
