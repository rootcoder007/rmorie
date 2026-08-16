% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{HrvRatio}
\alias{HrvRatio}
\title{LF/HF from the AR model PSD.  Often called a sympathovagal balance;}
\usage{
HrvRatio(rr, order = 16, fs = 4)
}
\arguments{
\item{rr}{See Usage.}
\item{order}{Defaults to \code{16}.}
\item{fs}{Defaults to \code{4}.}
}
\description{
that reading is contested -- HF is reasonably vagal, but LF reflects both branches plus the baroreflex.  The components are returned individually and in normalized units so no interpretation has to rest on the ratio alone.
}
\value{
A list with \code{lf_hf_ratio}, \code{lf}, \code{hf}, \code{vlf}, \code{total_power}, \code{lf_nu}, \code{hf_nu}, \code{order}, \code{bands}, \code{interpretation_caveat}, \code{method}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
HrvRatio(V)
}
