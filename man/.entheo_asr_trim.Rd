% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.entheo_asr_trim}
\alias{.entheo_asr_trim}
\title{Internal helper: Entheo Asr Trim}
\usage{
.entheo_asr_trim(x, threshold)
}
\description{
Internal helper: Entheo Asr Trim
}
\value{
A list with \code{arr}, \code{n_bad}.
}
\keyword{internal}
\examples{
M <- matrix(c(1, 2, 3, 4, 5, 6), nrow = 2)
.entheo_asr_trim(M, M)
}
