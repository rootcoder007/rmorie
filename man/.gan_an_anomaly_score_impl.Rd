% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.gan_an_anomaly_score_impl}
\alias{.gan_an_anomaly_score_impl}
\title{.gan_an_anomaly_score_impl}
\usage{
.gan_an_anomaly_score_impl(x, g_z, f_x, f_gz, lam = 0.1)
}
\arguments{
\item{x}{Passed to \code{.gan_an_residual_loss_impl}.}
\item{g_z}{Passed to \code{.gan_an_residual_loss_impl}.}
\item{f_x}{Passed to \code{.gan_an_discrimination_loss_impl}.}
\item{f_gz}{Passed to \code{.gan_an_discrimination_loss_impl}.}
\item{lam}{Defaults to \code{0.1}.}
}
\description{
A step of the gan_an_native implementation. Called by \code{anomaly_score}, \code{morie_gan_an}. See the file header for the source the module follows.
}
\value{
A list with \code{score}, \code{residual}, \code{discrimination}, \code{lambda}.
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
