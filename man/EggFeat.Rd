% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{EggFeat}
\alias{EggFeat}
\title{EggFeat: electrogastrogram dominant frequency and band powers}
\usage{
EggFeat(egg, fs, normal_band = c(0.0333, 0.0667))
}
\arguments{
\item{egg}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{fs}{Numeric; combined arithmetically in the body.}
\item{normal_band}{A vector; indexed elementwise. Defaults to \code{c(0.0333, 0.0667)}.}
}
\description{
A step of the rangayyan_phys implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A vector, from \code{c}.
}
\keyword{internal}
