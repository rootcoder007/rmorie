% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_clrnt}
\alias{morie_clrnt}
\title{morie_clrnt}
\usage{
morie_clrnt(clint_in_vitro, cl_h = NULL, fu_blood = NULL, log_pd = NULL, 
    fu_incubation = NULL, system = "hepatocytes", species = "human", liver_model = "well_stirred", 
    protein = 1, volume_ratio = 0.005, cl_plasma = NULL, fu_plasma = NULL, 
    blood_plasma_ratio = NULL, charge = "neutral", fold = 2) 
}
\arguments{
\item{clint_in_vitro}{Passed to \code{clrnt}.}
\item{cl_h}{Passed to \code{clrnt}.}
\item{fu_blood}{Passed to \code{clrnt}.}
\item{log_pd}{Passed to \code{clrnt}.}
\item{fu_incubation}{Passed to \code{clrnt}.}
\item{system}{Passed to \code{clrnt}. Defaults to \code{"hepatocytes"}.}
\item{species}{Passed to \code{clrnt}. Defaults to \code{"human"}.}
\item{liver_model}{Passed to \code{clrnt}. Defaults to \code{"well_stirred"}.}
\item{protein}{Passed to \code{clrnt}. Defaults to \code{1}.}
\item{volume_ratio}{Passed to \code{clrnt}. Defaults to \code{0.005}.}
\item{cl_plasma}{Passed to \code{clrnt}.}
\item{fu_plasma}{Passed to \code{clrnt}.}
\item{blood_plasma_ratio}{Passed to \code{clrnt}.}
\item{charge}{Passed to \code{clrnt}. Defaults to \code{"neutral"}.}
\item{fold}{Passed to \code{clrnt}. Defaults to \code{2}.}
}
\description{
A step of the clrnt_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
The value of \code{clrnt}.
}
\keyword{internal}
\examples{
r <- morie_clrnt(clint_in_vitro = 15, cl_h = 12, fu_blood = 0.1,
                 fu_incubation = 0.8)
str(r, max.level = 1)
}
