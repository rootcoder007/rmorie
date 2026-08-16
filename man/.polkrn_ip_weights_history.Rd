% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.polkrn_ip_weights_history}
\alias{.polkrn_ip_weights_history}
\title{Compute inverse-probability weights for a treatment history}
\usage{
.polkrn_ip_weights_history(A_hist, L_hist, kind, stabilize, trim)
}
\arguments{
\item{A_hist}{See Usage.}
\item{L_hist}{See Usage.}
\item{kind}{See Usage.}
\item{stabilize}{See Usage.}
\item{trim}{See Usage.}
}
\description{
Part of the polkrn_native implementation; see the file header for the source it follows.
}
\value{
A list with \code{w}, \code{per_time}.
}
\references{
----------
Hernan, M. A., Brumback, B. & Robins, J. M. (2002) "Estimating the
causal effect of zidovudine on CD4 count with a marginal structural
model for repeated measures", Statistics in Medicine 21(12),
1689-1709, doi:10.1002/sim.1144.

Robins, J. M., Hernan, M. A. & Brumback, B. (2000) "Marginal structural
models and causal inference in epidemiology", Epidemiology 11(5),
550-560, doi:10.1097/00001648-200009000-00011 -- the weights.

Hernan, M. A. & Robins, J. M. (2020) Causal Inference: What If,
Chapman & Hall/CRC, Sec. 21.2.
}
\keyword{internal}
