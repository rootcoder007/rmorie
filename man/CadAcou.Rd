% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{CadAcou}
\alias{CadAcou}
\title{CadAcou: coronary artery disease detection from acoustic signals}
\usage{
CadAcou(coronary_sound, fs, order = 8, hf_band = c(300, 900), ref_band = c(50, 300))
}
\arguments{
\item{coronary_sound}{See Usage.}
\item{fs}{Numeric; combined arithmetically in the body.}
\item{order}{Numeric; combined arithmetically in the body. Defaults to \code{8}.}
\item{hf_band}{A vector; indexed elementwise. Defaults to \code{c(300, 900)}.}
\item{ref_band}{A vector; indexed elementwise. Defaults to \code{c(50, 300)}.}
}
\description{
A step of the rangayyan_phys implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A vector, from \code{c}.
}
\keyword{internal}
