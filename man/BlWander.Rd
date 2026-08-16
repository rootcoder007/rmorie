% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{BlWander}
\alias{BlWander}
\title{Eqs (3.132)-(3.133): H(z) = (1/T)(1 - z^-1)/(1 - 0.995 z^-1).  The}
\usage{
BlWander(ecg, fs, pole = 0.995)
}
\arguments{
\item{ecg}{See Usage.}
\item{fs}{See Usage.}
\item{pole}{Defaults to \code{0.995}.}
}
\description{
zero at z = 1 kills DC (which is what baseline wander is); the pole close to it restores the gain to roughly unity by about 0.5 Hz so the QRS survives. The 1/T factor is applied as written, so the output is scaled by fs.
}
\value{
A list with \code{ecg_detrended}, \code{n}, \code{fs}, \code{pole}, \code{gain_dc}, \code{gain_at_half_hz}, \code{gain_at_nyquist}, \code{gain_relative_at_half_hz}, \code{dc_is_rejected}, \code{zero_at_z_equals_one}, \code{pole_restores_gain_above_the_wander_band}, \code{differentiates_by_the_one_over_T_factor}, \code{method}.
}
\keyword{internal}
\examples{
BlWander(ecg = c(1, 2, 3, 4, 5, 6, 7, 8), fs = 5L)
}
