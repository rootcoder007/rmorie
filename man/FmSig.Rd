% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{FmSig}
\alias{FmSig}
\title{Rangayyan names FM as a signal model but prints no equation for it,}
\usage{
FmSig(m, fc, fs, kf = 1, amplitude = 1) 
}
\arguments{
\item{m}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{fc}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{fs}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{kf}{Numeric; combined arithmetically in the body. Defaults to \code{1}.}
\item{amplitude}{Numeric; combined arithmetically in the body. Defaults to \code{1}.}
}
\description{
unlike AM in Section 5.5.1, so the standard definition is used and said so: y = A cos(2 pi fc t + 2 pi kf integral m), f_inst = fc + kf m. The phase integral is trapezoidal; a plain running sum biases it by half a sample of m at each end, which drifts over a long record.
}
\value{
A list with \code{y}, \code{phase}, \code{instantaneous_frequency}, \code{fc}, \code{fs}, \code{kf}, \code{max_instantaneous_frequency}, \code{min_instantaneous_frequency}, \code{aliases}, \code{method}.
}
\keyword{internal}
\examples{
FmSig(c(rep(0, 10), rep(50, 10)), fc = 100, fs = 1000, kf = 1)
}
