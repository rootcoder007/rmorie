% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_mfovsm}
\alias{morie_mfovsm}
\title{morie_mfovsm}
\usage{
morie_mfovsm(y, feature, A, H, v_in_numerator = TRUE, contrast = "cumulative", trim = NULL)
}
\arguments{
\item{y}{See Usage.}
\item{feature}{See Usage.}
\item{A}{See Usage.}
\item{H}{See Usage.}
\item{v_in_numerator}{Defaults to \code{TRUE}.}
\item{contrast}{Defaults to \code{"cumulative"}.}
\item{trim}{Defaults to \code{NULL}.}
}
\description{
Part of the mfovsm_native implementation; see the file header for the source it follows.
}
\value{
A list with \code{estimate}, \code{se}, \code{main_effect}, \code{main_effect_se}, \code{feature_effect}, \code{intercept}, \code{coef}, \code{vcov}, \code{weights}, \code{mean_weight}, \code{max_weight}, \code{effective_sample_size}, \code{exposure}, \code{v_in_numerator}, \code{n}, \code{n_times}, \code{contrast}, \code{method}.
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
