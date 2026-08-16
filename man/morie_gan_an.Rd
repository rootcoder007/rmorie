% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_gan_an}
\alias{morie_gan_an}
\title{morie_gan_an}
\usage{
morie_gan_an(x, generator, feature_fn, z_dim, steps = 200, lr = 0.05, lam = 0.1, seed = 0, h = 1e-04, step_decay = 0.05)
}
\arguments{
\item{x}{Passed to \code{.gan_an_anomaly_score_impl}.}
\item{generator}{Accepted by the signature and not used anywhere in the body.}
\item{feature_fn}{Accepted by the signature and not used anywhere in the body.}
\item{z_dim}{Coerced to integer by the body, with \code{as.integer}.}
\item{steps}{Coerced to integer by the body, with \code{as.integer}. Defaults to \code{200}.}
\item{lr}{Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{0.05}.}
\item{lam}{Passed to \code{.gan_an_anomaly_score_impl}. Defaults to \code{0.1}.}
\item{seed}{Coerced to integer by the body, with \code{as.integer}. Defaults to \code{0}.}
\item{h}{Numeric; combined arithmetically in the body. Defaults to \code{1e-04}.}
\item{step_decay}{Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{0.05}.}
}
\description{
A step of the gan_an_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A list with \code{estimate}, \code{score}, \code{z}, \code{reconstruction}, \code{loss_history}, \code{residual}, \code{discrimination}, \code{final_step}, \code{method}, \code{note}.
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
