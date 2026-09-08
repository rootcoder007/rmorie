% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_geron_mixed_precision}
\alias{morie_geron_mixed_precision}
\title{Mixed-precision training plan: FP16 forward, FP32 master weights (Geron Ch 17, hmmxp2)}
\usage{
morie_geron_mixed_precision(model, loss_scale = 1024, grads = NULL) 
}
\arguments{
\item{model}{Numeric vector or named list of numeric vectors (weights).}
\item{loss_scale}{Positive finite scale.}
\item{grads}{Optional gradients matching `model`.}
}
\description{
No native float16 in base R; the underflow/overflow analysis and the loss-scale recommendation are computed at full double precision, which is exact for the summary statistics (they only compare |g| against the FP16 thresholds 65504 / 6.103515625e-05).
}
\value{
A list with \code{fp16_weights}, \code{overflow}, \code{weight_overflow}, \code{n_underflow}, \code{loss_scale}, \code{max_safe_loss_scale}, \code{recommended_loss_scale}, \code{memory_bytes_fp32}, \code{memory_bytes_fp16}, \code{fp16_max}, \code{fp16_min_normal}, \code{estimate}, \code{n}, \code{method}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
morie_geron_mixed_precision(V)
}
