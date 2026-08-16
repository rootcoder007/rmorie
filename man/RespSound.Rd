% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{RespSound}
\alias{RespSound}
\title{RespSound: respiratory sound generation model (bronchial turbulence);}
\usage{
RespSound(length_m = 0.1, radius_m = 0.009, freqs = NULL, rho = 1.2, c = 343, mu = 1.8e-05, P0 = 101325, eta = 1.4, lam = 0.026, cp = 1005)
}
\arguments{
\item{length_m}{Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{0.1}.}
\item{radius_m}{Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{0.009}.}
\item{freqs}{Optional; may be \code{NULL}. Coerced to numeric by the body, with \code{as.numeric}.}
\item{rho}{Numeric; passed to \code{min}. Defaults to \code{1.2}.}
\item{c}{Numeric; passed to \code{min}. Defaults to \code{343}.}
\item{mu}{Numeric; passed to \code{min}. Defaults to \code{1.8e-05}.}
\item{P0}{Numeric; passed to \code{min}. Defaults to \code{101325}.}
\item{eta}{Numeric; combined arithmetically in the body. Defaults to \code{1.4}.}
\item{lam}{Numeric; passed to \code{min}. Defaults to \code{0.026}.}
\item{cp}{Numeric; passed to \code{min}. Defaults to \code{1005}.}
}
\description{
Rangayyan (2024) eqs. (7.122), (7.127), (7.128), (7.129), Section 7.7.1.
}
\value{
A list with \code{freq_hz}, \code{transfer_mag}, \code{transfer_db}, \code{La_kg_per_m4}, \code{Ca_m3_per_Pa}, \code{Ra_Pa_s_per_m3}, \code{Ga_m3_per_Pa_s}, \code{resonance_hz}, \code{area_m2}, \code{circumference_m}, \code{volume_m3}, \code{units}, \code{method}.
}
\keyword{internal}
\examples{
RespSound()
}
