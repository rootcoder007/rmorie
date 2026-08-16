% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.vdcal_fut_descriptors}
\alias{.vdcal_fut_descriptors}
\title{The Lombardo descriptor route, on caller-supplied coefficients:}
\usage{
.vdcal_fut_descriptors(elogd, fi, fu, coefficients)
}
\arguments{
\item{elogd}{See Usage.}
\item{fi}{See Usage.}
\item{fu}{Numeric; combined arithmetically in the body.}
\item{coefficients}{Optional; may be \code{NULL}. A vector; its length is taken and its elements indexed.}
}
\description{
log(1/fut) = a + b ElogD(7.4) + c fi(7.4) + d log(1/fu) The four coefficients are fitted quantities that belong to the papers. This module will not guess them: pass them, or use the measured route.
}
\value{
The value of \code{fut}, as built in the body.
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
