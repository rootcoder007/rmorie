% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_fwxF}
\alias{morie_fwxF}
\title{morie_fwxF}
\usage{
morie_fwxF(temp, rh, wind, rain, month, ffmc_init = 85, dmc_init = 6, dc_init = 15) 
}
\arguments{
\item{temp}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{rh}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{wind}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{rain}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{month}{A vector; its length is taken.}
\item{ffmc_init}{Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{85}.}
\item{dmc_init}{Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{6}.}
\item{dc_init}{Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{15}.}
}
\description{
A step of the fwxF_mixedcase_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
The value of \code{out}, as built in the body.
}
\keyword{internal}
\examples{
morie_fwxF(temp = c(1, 2, 3, 4, 5, 6, 7, 8), rh = c(1, 2, 3, 4, 5, 6, 7, 8), wind = c(1, 2, 3, 4, 5, 6, 7, 8), rain = c(1, 2, 3, 4, 5, 6, 7, 8), month = c(1, 2, 3, 4, 5, 6, 7, 8))
}
