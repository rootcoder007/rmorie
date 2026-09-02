% Generated from the module source by ledger/wave3/gen_rd_any.R --
% roxygen2 hangs on this package. Usage comes from args(), so it
% tracks the formals.
\name{morie_vdcal}
\alias{morie_vdcal}
\title{Volume of distribution at steady state, forwards or backwards}
\usage{
morie_vdcal(smiles, ppb, fut = NULL, vss = NULL, direction = "vss", weight = 70, 
    par = NULL, elogd = NULL, fi = NULL, coefficients = NULL) 
}
\arguments{
\item{smiles}{Carried through untouched. The model is physiological,   not structural: nothing here reads the structure, and pretending   otherwise would be the fabrication this module exists to avoid.}
\item{ppb}{Plasma protein binding as the FREE fraction, in (0, 1]. A   drug quoted as 99 percent bound has fu = 0.01.}
\item{fut}{The tissue free fraction, for the forward direction, or   NULL.}
\item{vss}{A measured volume in litres per kilogram, for the inverse   direction, or NULL.}
\item{direction}{A member of the direction list.}
\item{weight}{Body mass, used only to report the volume in litres as   well as litres per kilogram.}
\item{par}{Overrides for the physiological constants, or NULL.}
\item{elogd}{The lipophilicity descriptor, or NULL.}
\item{fi}{The ionised fraction at pH 7.4, or NULL.}
\item{coefficients}{The fitted regression coefficients, or NULL.}
}
\description{
Volume of distribution at steady state, forwards or backwards
}
\value{
A list with the volume in both units, the three terms, the   tissue free fraction used or solved for, and the route taken.
}
\references{
  Oie, S. and Tozer, T.N. (1979) "Effect of altered plasma protein
    binding on apparent volume of distribution." Journal of
    Pharmaceutical Sciences 68(9), 1203-1205.
    doi:10.1002/jps.2600680948.
  Lombardo, F., Obach, R.S., Shalaeva, M.Y. and Gao, F. (2002)
    "Prediction of volume of distribution values in humans for neutral
    and basic drugs using physicochemical measurements and plasma
    protein binding data." Journal of Medicinal Chemistry 45(13),
    2867-2876.
  Waters, N.J. and Lombardo, F. (2010) "Use of the Oie-Tozer model in
    understanding mechanisms and determinants of drug distribution."
    Drug Metabolism and Disposition 38(7), 1094-1102.
    doi:10.1124/dmd.110.032722.
  Lombardo, F., Berellini, G. and Obach, R.S. (2019) "An accurate in
    vitro prediction of human VDss based on the Oie-Tozer equation and
    primary physicochemical descriptors. 3." Drug Metabolism and
    Disposition 47(12), 1380-1387.
}
\keyword{internal}
\examples{
morie_vdcal("CCO", 0.02, fut = 0.005)
}
