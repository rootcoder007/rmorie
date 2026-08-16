% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.mfovsm_hist}
\alias{.mfovsm_hist}
\title{.mfovsm_hist}
\usage{
.mfovsm_hist(obj, allow_one = FALSE)
}
\arguments{
\item{obj}{Optional; may be \code{NULL}. A list; the body checks with \code{is.list}.}
\item{allow_one}{A flag; the body branches on it. Defaults to \code{FALSE}.}
}
\description{
A step of the mfovsm_native implementation. Called by \code{morie_mfovsm}. See the file header for the source the module follows.
}
\value{
The value of \code{list}.
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
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.mfovsm_hist(V)
}
