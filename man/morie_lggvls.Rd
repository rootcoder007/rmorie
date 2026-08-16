% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_lggvls}
\alias{morie_lggvls}
\title{IPTW for a sustained exposure, with lagged values in the treatment}
\usage{
morie_lggvls(y, A, H, lag = 1, Y_hist = NULL, stabilize = TRUE, kind = "binary", trim = NULL, contrast = "cumulative")
}
\arguments{
\item{y}{See Usage.}
\item{A}{See Usage.}
\item{H}{See Usage.}
\item{lag}{Defaults to \code{1}.}
\item{Y_hist}{Defaults to \code{NULL}.}
\item{stabilize}{Defaults to \code{TRUE}.}
\item{kind}{Defaults to \code{"binary"}.}
\item{trim}{Defaults to \code{NULL}.}
\item{contrast}{Defaults to \code{"cumulative"}.}
}
\description{
model Part of the lggvls_native implementation; see the file header for the source it follows.
}
\value{
A list with \code{estimate}, \code{se}, \code{intercept}, \code{coef}, \code{vcov}, \code{weights}, \code{mean_weight}, \code{max_weight}, \code{effective_sample_size}, \code{cumulative_exposure}, \code{per_time}, \code{n_times}, \code{lag}, \code{n}, \code{contrast}, \code{method}.
}
\references{
----------
Robins, J. (1986) "A new approach to causal inference in mortality
studies with a sustained exposure period -- application to control of
the healthy worker survivor effect", Mathematical Modelling 7(9-12),
1393-1512, doi:10.1016/0270-0255(86)90088-6.

Hernan, M. A. & Robins, J. M. (2020) Causal Inference: What If, Boca
Raton: Chapman & Hall/CRC, Sec. 21.2 for the product-over-time weights
and Ch. 20 for why conditioning fails where weighting works.
}
\keyword{internal}
\examples{
morie_lggvls(y = c(1, 2, 3, 4, 5, 6, 7, 8), A = c(1, 2, 3, 4, 5, 6, 7, 8), H = 0.5)
}
