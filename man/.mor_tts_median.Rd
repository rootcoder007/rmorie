% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.mor_tts_median}
\alias{.mor_tts_median}
\title{.mor_tts_median}
\usage{
.mor_tts_median(v)
}
\arguments{
\item{v}{Numeric; passed to \code{sort}.}
}
\description{
A step of the ttsAn_native implementation. Called by \code{.mor_tts_esd}, \code{morie_ttsAn}. See the file header for the source the module follows.
}
\value{
One of two values, depending on the branch taken.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.mor_tts_median(V)
}
