% Generated from the module source by ledger/wave3/gen_rd_any.R --
% roxygen2 hangs on this package. Usage comes from args(), so it
% tracks the formals.
\name{morie_vdcal_fut}
\alias{morie_vdcal_fut}
\title{Solve the model backwards for the tissue free fraction}
\usage{
morie_vdcal_fut(vss, fu, par = NULL)
}
\arguments{
\item{vss}{The measured volume, in litres per kilogram.}
\item{fu}{The plasma free fraction.}
\item{par}{Overrides for the physiological constants, or NULL.}
}
\description{
Closed form, not a search: the volume is linear in fu/fut, so inverting it is arithmetic. A measured volume smaller than the plasma-and-albumin floor cannot be produced by this model at any tissue binding, and that is an error rather than a negative fut.
}
\value{
The implied tissue free fraction.
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
