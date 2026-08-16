% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{PCepstrum}
\alias{PCepstrum}
\title{Eq (4.81).  The book notes the final squaring is omitted in some}
\usage{
PCepstrum(x, square = TRUE)
}
\arguments{
\item{x}{See Usage.}
\item{square}{Defaults to \code{TRUE}.}
}
\description{
definitions, and that it matters: WITH the square, eq (4.82) holds only when the cross-term is negligible; WITHOUT it, no cross-term arises and eq (4.82) is exact.
}
\value{
A list with \code{cepstrum}, \code{unsquared}, \code{log_power}, \code{n}, \code{squared}, \code{zero_bins}, \code{retains_phase}, \code{additivity_exact}, \code{method}.
}
\keyword{internal}
