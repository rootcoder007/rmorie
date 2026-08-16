% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_geron_quantization_aware_training_hm}
\alias{morie_geron_quantization_aware_training_hm}
\title{Quantization-aware training with a straight-through estimator (Geron App B, hmqat)}
\usage{
morie_geron_quantization_aware_training_hm(model, X, y, epochs = 200, lr = 0.1, bits = 8)
}
\arguments{
\item{model}{Initial full-precision weights.}
\item{X,y}{Data.}
\item{epochs,lr,bits}{Controls.}
}
\description{
Quantization-aware training with a straight-through estimator (Geron App B, hmqat)
}
\value{
A list with \code{weights}, \code{quantized_weights}, \code{scale}, \code{loss}, \code{fp_loss}, \code{loss_history}, \code{bits}, \code{estimate}, \code{n}, \code{method}.
}
\keyword{internal}
