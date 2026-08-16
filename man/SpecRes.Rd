% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{SpecRes}
\alias{SpecRes}
\title{Rayleigh: delta_f = fs/N, set by the RECORD LENGTH.  Zero-padding}
\usage{
SpecRes(n, fs = 1, window = "rectangular")
}
\arguments{
\item{n}{See Usage.}
\item{fs}{Defaults to \code{1}.}
\item{window}{Character; passed to \code{tolower}. Defaults to \code{"rectangular"}.}
}
\description{
interpolates the same spectrum more finely and does NOT improve resolution -- the commonest misreading of a smoother-looking plot. Every window that suppresses leakage widens the main lobe, so a window buys dynamic range with resolution.
}
\value{
A list with \code{delta_f}, \code{resolution}, \code{main_lobe_bins}, \code{sidelobe_db}, \code{equivalent_noise_bandwidth_bins}, \code{duration}, \code{n}, \code{fs}, \code{window}, \code{zero_padding_helps}, \code{method}.
}
\keyword{internal}
\examples{
SpecRes(n = 5L)
}
