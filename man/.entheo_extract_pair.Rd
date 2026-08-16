% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.entheo_extract_pair}
\alias{.entheo_extract_pair}
\title{Internal helper: Entheo Extract Pair}
\usage{
.entheo_extract_pair(record_or_eeg, fmri)
}
\description{
Internal helper: Entheo Extract Pair
}
\value{
A list with \code{e_dmt}, \code{f_dmt}, \code{e_pcb}, \code{f_pcb}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.entheo_extract_pair(V, V)
}
