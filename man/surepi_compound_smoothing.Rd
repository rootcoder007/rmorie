% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{surepi_compound_smoothing}
\alias{surepi_compound_smoothing}
\title{surepi_compound_smoothing}
\usage{
surepi_compound_smoothing(values, current, passes = c(4, 2, 5, 3), multiplier = 2)
}
\arguments{
\item{values}{See Usage.}
\item{current}{See Usage.}
\item{passes}{Defaults to \code{c(4, 2, 5, 3)}.}
\item{multiplier}{Defaults to \code{2}.}
}
\description{
Part of the surepi_native implementation; see the file header for the source it follows.
}
\value{
A list with \code{smoothed}, \code{baseline}, \code{sigma}, \code{threshold}, \code{flag}, \code{current}, \code{method}.
}
\references{
----------
Hutwagner, L., Thompson, W., Seeman, G. M. & Treadwell, T. (2003)
"The Bioterrorism Preparedness and Response Early Aberration
Reporting System (EARS)", Journal of Urban Health: Bulletin of the
New York Academy of Medicine 80(2, Supplement 1), i89-i96.

Hutwagner, L. C., Maloney, E. K., Bean, N. H., Slutsker, L. & Martin,
S. M. (1997) "Using laboratory-based surveillance data for
prevention: an algorithm for detecting Salmonella outbreaks",
Emerging Infectious Diseases 3(3), 395-400.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
surepi_compound_smoothing(V, V)
}
