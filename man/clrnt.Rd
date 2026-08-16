% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{clrnt}
\alias{clrnt}
\title{clrnt}
\usage{
clrnt(clint_in_vitro, cl_h = NULL, fu_blood = NULL, log_pd = NULL, fu_incubation = NULL, system = "hepatocytes", species = "human", liver_model = "well_stirred", protein = 1, volume_ratio = 0.005, cl_plasma = NULL, fu_plasma = NULL, blood_plasma_ratio = NULL, charge = "neutral", fold = 2)
}
\arguments{
\item{clint_in_vitro}{A vector; its length is taken.}
\item{cl_h}{Defaults to \code{NULL}.}
\item{fu_blood}{Defaults to \code{NULL}.}
\item{log_pd}{The body requires: clrnt: give either fu_incubation or log_pd so equations 1-2 can estimate it.}
\item{fu_incubation}{The body requires: clrnt: give either fu_incubation or log_pd so equations 1-2 can estimate it.}
\item{system}{Compared against \code{"microsomes"}. Defaults to \code{"hepatocytes"}.}
\item{species}{Carried through into a list the body builds. Defaults to \code{"human"}.}
\item{liver_model}{Carried through into a list the body builds. Defaults to \code{"well_stirred"}.}
\item{protein}{Iterated over elementwise, with \code{vapply}. Defaults to \code{1}.}
\item{volume_ratio}{Iterated over elementwise, with \code{vapply}. Defaults to \code{0.005}.}
\item{cl_plasma}{Defaults to \code{NULL}.}
\item{fu_plasma}{The body requires: clrnt: plasma clearance needs fu_plasma too.}
\item{blood_plasma_ratio}{Defaults to \code{NULL}.}
\item{charge}{Defaults to \code{"neutral"}.}
\item{fold}{Defaults to \code{2}.}
}
\description{
A step of the clrnt_native implementation. Called by \code{morie_clrnt}. See the file header for the source the module follows.
}
\value{
The value of \code{out}, as built in the body.
}
\keyword{internal}
