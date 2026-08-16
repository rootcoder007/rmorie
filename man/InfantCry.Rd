% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{InfantCry}
\alias{InfantCry}
\title{InfantCry: cry F0 track, melody coding and formants}
\usage{
InfantCry(cry, fs, window_ms = 40, f0_range = c(200, 1000), order = NULL, flat_tolerance = 0.06)
}
\arguments{
\item{cry}{See Usage.}
\item{fs}{Numeric; combined arithmetically in the body.}
\item{window_ms}{Numeric; combined arithmetically in the body. Defaults to \code{40}.}
\item{f0_range}{A vector; indexed elementwise. Defaults to \code{c(200, 1000)}.}
\item{order}{Defaults to \code{NULL}.}
\item{flat_tolerance}{Defaults to \code{0.06}.}
}
\description{
A step of the rangayyan_phys implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A list with \code{f0_track_hz}, \code{t_track_s}, \code{melody}, \code{melody_units}, \code{mean_f0_hz}, \code{sd_f0_hz}, \code{min_f0_hz}, \code{max_f0_hz}, \code{f0_range_semitones}, \code{voiced_fraction}, \code{in_common_band_fraction}, \code{high_pitched}, \code{formants_hz}, \code{window_ms}, \code{n_windows}, \code{fs_hz}, \code{flat_tolerance}, \code{units}, \code{method}.
}
\keyword{internal}
