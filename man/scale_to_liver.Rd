% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{scale_to_liver}
\alias{scale_to_liver}
\title{scale_to_liver}
\usage{
scale_to_liver(clint_in_vitro, fu_incubation, system = "hepatocytes", species = "human", pbsf = NULL, liver_weight = NULL)
}
\arguments{
\item{clint_in_vitro}{See Usage.}
\item{fu_incubation}{See Usage.}
\item{system}{One of \code{"hepatocytes"}, \code{"microsomes"}. Defaults to \code{"hepatocytes"}.}
\item{species}{Defaults to \code{"human"}.}
\item{pbsf}{Defaults to \code{NULL}.}
\item{liver_weight}{Defaults to \code{NULL}.}
}
\description{
A step of the clrnt_native implementation. Called by \code{clrnt}. See the file header for the source the module follows.
}
\value{
A numeric value.
}
\keyword{internal}
