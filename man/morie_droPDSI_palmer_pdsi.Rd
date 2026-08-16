% Generated from the module source by ledger/gen_rd.R -- roxygen2 hangs
% on this package. Usage comes from args(), so it tracks the formals.
\name{morie_droPDSI_palmer_pdsi}
\alias{morie_droPDSI_palmer_pdsi}
\alias{morie_droPDSI}
\title{Palmer Drought Severity Index from a two-layer water}
\usage{
morie_droPDSI_palmer_pdsi(precip, pet, awc = 100, month = NULL)
}
\description{
R arm of droPDSI -- Palmer Drought Severity Index from a two-layer water U.S. Weather Bureau. Mirrors src/morie/fn/droPDSI.py.
}
\value{
A list with components \code{n}, \code{mon}, \code{estimate}, \code{pdsi}, \code{z_index}, \code{departure}, \code{cafec_precip}, \code{alpha}, \code{beta}, \code{gamma}, \code{delta}, \code{K}, \code{K_month}, \code{mean_abs_departure}, \code{evapotranspiration}, \code{recharge}, \code{runoff}, \code{loss}, \code{soil_surface_capacity}, \code{soil_under_capacity}, \code{duration_factor}, \code{duration_divisor}, \code{method}, \code{note}.
}
\references{
balance. Palmer, W. C. (1965) Meteorological Drought, Research Paper No. 45,
}
\keyword{internal}
