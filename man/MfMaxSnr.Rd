% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{MfMaxSnr}
\alias{MfMaxSnr}
\title{Eq (4.46) rearranged: M_y^2 / P_eta_o = 2 E_x / P_eta_i, the familiar}
\usage{
MfMaxSnr(x, noise_power, t = NULL, dt = 1)
}
\arguments{
\item{x}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{noise_power}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{t}{Optional; may be \code{NULL}. Coerced to numeric by the body, with \code{as.numeric}.}
\item{dt}{Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{1}.}
}
\description{
2E/N0.  It depends on the signal ONLY through its energy -- the same detectability from a short loud transient as from a long quiet one of equal energy.  That is the substantive content of the theorem.
}
\value{
A list with \code{snr}, \code{snr_db}, \code{energy}, \code{noise_power}, \code{n0}, \code{depends_only_on_energy}, \code{method}.
}
\keyword{internal}
\examples{
MfMaxSnr(x = c(1, 2, 3, 4, 5, 6, 7, 8), noise_power = 5L)
}
