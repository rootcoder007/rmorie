% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{anomaly_score}
\alias{anomaly_score}
\title{anomaly_score}
\usage{
anomaly_score(x, g_z, f_x, f_gz, lam = 0.1)
}
\arguments{
\item{x}{See Usage.}
\item{g_z}{See Usage.}
\item{f_x}{See Usage.}
\item{f_gz}{See Usage.}
\item{lam}{Defaults to \code{0.1}.}
}
\description{
Part of the gan_an_native implementation; see the file header for the source it follows.
}
\value{
The value of \code{.gan_an_anomaly_score_impl}.
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
anomaly_score(x = c(1, 2, 3, 4, 5, 6, 7, 8), g_z = c(1, 2, 3, 4, 5, 6, 7, 8), f_x = c(1, 2, 3, 4, 5, 6, 7, 8), f_gz = c(1, 2, 3, 4, 5, 6, 7, 8))
}
