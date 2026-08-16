% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.mfovsm_ip_weights}
\alias{.mfovsm_ip_weights}
\title{.mfovsm_ip_weights}
\usage{
.mfovsm_ip_weights(ak, den, num, kind = "binary", stabilize = TRUE)
}
\arguments{
\item{ak}{See Usage.}
\item{den}{See Usage.}
\item{num}{See Usage.}
\item{kind}{Defaults to \code{"binary"}.}
\item{stabilize}{Defaults to \code{TRUE}.}
}
\description{
Part of the mfovsm_native implementation; see the file header for the source it follows.
}
\value{
A list with \code{weights}, \code{fitted}.
}
\references{
----------
Robins, J. M. & Hernan, M. A. (2009) "Estimation of the causal effects
of time-varying exposures", in Fitzmaurice, G., Davidian, M.,
Verbeke, G. & Molenberghs, G. (eds.), Longitudinal Data Analysis,
Chapman & Hall/CRC Handbooks of Modern Statistical Methods, 553-599,
doi:10.1201/9781420011579.ch23.

Hernan, M. A. & Robins, J. M. (2020) Causal Inference: What If,
Chapman & Hall/CRC, Sec. 12.5 (effect modification and marginal
structural models) and Sec. 21.2 (the time-varying weights).
}
\keyword{internal}
