% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{BlackmanW}
\alias{BlackmanW}
\title{The Blackman window}
\usage{
BlackmanW(N)
}
\arguments{
\item{N}{The window length, at least one. A length of one is the   degenerate window and is returned as such rather than divided by   zero.}
}
\description{
A third cosine term buys much deeper sidelobes than the Hamming window -- about minus fifty-eight decibels -- at the cost of a main lobe half again as wide. Resolution against leakage is the trade every window makes and none escapes; this one sits at the leakage end of it.
}
\value{
A list with the window \code{w}, its length, its sum, its two   endpoints and its coherent gain, together with the note that it has   the widest main lobe of the three classical windows.
}
\keyword{internal}
\examples{
BlackmanW(N = 5L)
}
