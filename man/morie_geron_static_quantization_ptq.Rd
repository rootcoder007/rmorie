% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_geron_static_quantization_ptq}
\alias{morie_geron_static_quantization_ptq}
\title{Static post-training quantization (PTQ) from calibration ranges (Geron App B, hmptq)}
\usage{
morie_geron_static_quantization_ptq(model, calibration_data, bits = 8, percentile = 100)
}
\arguments{
\item{model}{Weights.}
\item{calibration_data}{Representative activations.}
\item{bits}{Bit width, 2 to 16.}
\item{percentile}{Range-clipping percentile in (0, 100\].}
}
\description{
Static post-training quantization (PTQ) from calibration ranges (Geron App B, hmptq)
}
\value{
A list with \code{quantized_weights}, \code{dequantized_weights}, \code{weight_scale}, \code{activation_scale}, \code{zero_point}, \code{activation_range}, \code{max_weight_error}, \code{compression}, \code{bits}, \code{estimate}, \code{n}, \code{method}.
}
\keyword{internal}
