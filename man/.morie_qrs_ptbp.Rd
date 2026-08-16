% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_qrs_ptbp}
\alias{.morie_qrs_ptbp}
\title{Pan-Tompkins bandpass: eq (4.8) lowpass then eq (4.13) highpass.  The}
\usage{
.morie_qrs_ptbp(x)
}
\arguments{
\item{x}{A vector; its length is taken and its elements indexed.}
}
\description{
coefficients are integers tied to fs = 200 Hz (book, Section 4.3.2) and are NOT rescaled here.
}
\value{
The value of \code{p}, as built in the body.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.morie_qrs_ptbp(V)
}
