% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.tdcvar_ip_weights_history}
\alias{.tdcvar_ip_weights_history}
\title{.tdcvar_ip_weights_history}
\usage{
.tdcvar_ip_weights_history(A_hist, L_hist, kind = "binary", stabilize = TRUE, trim = NULL)
}
\arguments{
\item{A_hist}{See Usage.}
\item{L_hist}{See Usage.}
\item{kind}{Defaults to \code{"binary"}.}
\item{stabilize}{Defaults to \code{TRUE}.}
\item{trim}{Defaults to \code{NULL}.}
}
\description{
Part of the tdcvar_native implementation; see the file header for the source it follows.
}
\value{
A list with \code{weights}, \code{per_time}.
}
\references{
----------
Hernan, M. A. & Robins, J. M. (2020) Causal Inference: What If, Boca
Raton: Chapman & Hall/CRC. Ch. 20 (treatment-confounder feedback),
Sec. 21.2 (IP weighting for time-varying treatments), Sec. 12.3 (the
stabilized weights and the mean-1 diagnostic).

Robins, J. (1986) "A new approach to causal inference in mortality
studies with a sustained exposure period", Mathematical Modelling
7(9-12), 1393-1512, doi:10.1016/0270-0255(86)90088-6 -- where the
problem and the weighted solution were first set out.
}
\keyword{internal}
