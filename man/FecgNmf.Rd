% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{FecgNmf}
\alias{FecgNmf}
\title{FecgNmf}
\usage{
FecgNmf(x, fs, nwin = 64, hop = NULL, rank = 4, lam = 0, maxiter = 150, taum = 0.6, tauf = 0.45, seed = 1)
}
\arguments{
\item{x}{A vector; its length is taken.}
\item{fs}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{nwin}{Numeric; combined arithmetically in the body. Defaults to \code{64}.}
\item{hop}{Optional; may be \code{NULL}. Passed to \code{.morie_bx_stft}.}
\item{rank}{Passed to \code{.morie_bx_nmfmu}. Defaults to \code{4}.}
\item{lam}{Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{0}.}
\item{maxiter}{Passed to \code{.morie_bx_nmfmu}. Defaults to \code{150}.}
\item{taum}{Defaults to \code{0.6}.}
\item{tauf}{Defaults to \code{0.45}.}
\item{seed}{Passed to \code{.morie_bx_nmfmu}. Defaults to \code{1}.}
}
\description{
A step of the rangayyan_class2 implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A list with \code{fetal}, \code{maternal}, \code{fetalrow}, \code{maternalrow}, \code{peaks}, \code{W}, \code{H}, \code{error}, \code{method}.
}
\keyword{internal}
