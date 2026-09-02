% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_snmcox_blip_down}
\alias{morie_snmcox_blip_down}
\title{The blipped-down (untreated) time U(psi) = int_0^T exp(psi A(u)) du}
\usage{
morie_snmcox_blip_down(time, treat_times, psi) 
}
\description{
The blipped-down (untreated) time U(psi) = int_0^T exp(psi A(u)) du.
}
\arguments{
\item{time}{Argument \code{time}; see Usage.}
\item{treat_times}{Argument \code{treat_times}; see Usage.}
\item{psi}{Argument \code{psi}; see Usage.}
}
\value{
A numeric value.
}
\references{
Robins, J. M. (1992) "Estimation of the time-dependent accelerated failure
time model in the presence of confounding factors", Biometrika 79(2),
321-334, doi:10.1093/biomet/79.2.321.
Robins, J. M., Blevins, D., Ritter, G. and Wulfsohn, M. (1992) "G-estimation
of the effect of prophylaxis therapy for Pneumocystis carinii pneumonia on
the survival of AIDS patients", Epidemiology 3(4), 319-336,
doi:10.1097/00001648-199207000-00007.
Hernan, M. A., Cole, S. R., Margolick, J., Cohen, M. and Robins, J. M. (2005)
"Structural accelerated failure time models for survival analysis in studies
with time-varying treatments", Pharmacoepidemiology and Drug Safety 14(7),
477-491, doi:10.1002/pds.1064.
}
\keyword{internal}
\examples{
morie_snmcox_blip_down(time = 5L, treat_times = data.frame(x = c(1, 2, 3, 4), y = c(2, 4, 5, 9)), psi = c(1, 2, 3, 4, 5, 6, 7, 8))
}
