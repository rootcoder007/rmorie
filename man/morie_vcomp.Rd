% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_vcomp}
\alias{morie_vcomp}
\title{morie_vcomp}
\usage{
morie_vcomp(y, group, method = "reml", conf_level = 0.95)
}
\arguments{
\item{y}{See Usage.}
\item{group}{See Usage.}
\item{method}{Defaults to \code{"reml"}.}
\item{conf_level}{Defaults to \code{0.95}.}
}
\description{
Part of the vcomp_native implementation; see the file header for the source it follows.
}
\value{
A list with \code{sigma2_a}, \code{sigma2_e}, \code{icc}, \code{icc_lower}, \code{icc_upper}, \code{method_used}, \code{balanced}, \code{a}, \code{N}, \code{fit}, \code{method}.
}
\keyword{internal}
