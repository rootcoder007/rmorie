% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_droPDSI_palmer_pdsi}
\alias{morie_droPDSI_palmer_pdsi}
\title{morie_droPDSI_palmer_pdsi}
\usage{
morie_droPDSI_palmer_pdsi(precip, pet, awc = 100, month = NULL)
}
\arguments{
\item{precip}{See Usage.}
\item{pet}{See Usage.}
\item{awc}{Defaults to \code{100}.}
\item{month}{Defaults to \code{NULL}.}
}
\description{
Part of the droPDSI_native implementation; see the file header for the source it follows.
}
\value{
A list with \code{estimate}, \code{pdsi}, \code{z_index}, \code{departure}, \code{cafec_precip}, \code{alpha}, \code{beta}, \code{gamma}, \code{delta}, \code{K}, \code{K_month}, \code{mean_abs_departure}, \code{evapotranspiration}, \code{recharge}, \code{runoff}, \code{loss}, \code{soil_surface_capacity}, \code{soil_under_capacity}, \code{n}, \code{duration_factor}, \code{duration_divisor}, \code{method}, \code{note}.
}
\keyword{internal}
